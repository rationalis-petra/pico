
;; ---------------------------------------------------
;; 
;;           Interactive Relic Editor (IRE)
;; 
;; ---------------------------------------------------


(open platform)
(open data)
(open data.list)

(def max-frame-in-flight 2)

(def FrameObject Struct
  [.command-buffer hedron.CommandBuffer]
  [.in-flight hedron.Fence])

(def SyncObject Struct
  [.image-available hedron.Semaphore]
  [.render-finished hedron.Semaphore])

(def create-frame-object proc [pool] struct
  [.command-buffer (hedron.create-command-buffer pool)]
  [.in-flight (hedron.create-fence)])

(def destroy-sync-object proc [(sync SyncObject)] seq
  (hedron.destroy-semaphore sync.image-available)
  (hedron.destroy-semaphore sync.render-finished))

(def create-frame-objects proc [pool] 
  (list (create-frame-object pool) (create-frame-object pool)))

(def destroy-frame-object proc [(fdata FrameObject)] seq
  (hedron.destroy-fence fdata.in-flight))

(def create-sync-object proc [] struct
  [.image-available (hedron.create-semaphore)]
  [.render-finished (hedron.create-semaphore)])

(def create-sync-objects proc [(number-elements U64)] seq
  [let! sync-objects (mk-list number-elements number-elements)]
  (loop [for i from 0 below number-elements]
    (eset i (create-sync-object) sync-objects))
  sync-objects)


(def load-shader proc [filename] seq
  [let! file (filesystem.open-file filename :read)]
  (hedron.create-shader-module (filesystem.read-chunk file :none)))

;; -------------------------------------------------------------------
;;
;;             Drawing and related utility functions
;; 
;; -------------------------------------------------------------------

(def create-graphics-pipeline proc [surface] seq
  [let! ;; shaders 
        vert-shader (load-shader "vert.spv")
        frag-shader (load-shader "frag.spv")]

  [let! pipeline
    (hedron.create-pipeline (list vert-shader frag-shader) surface)]

  (hedron.destroy-shader-module vert-shader)
  (hedron.destroy-shader-module frag-shader)
  pipeline)

(def record-command proc [command-buffer pipeline surface next-image] seq
  (hedron.command-begin command-buffer)
  (hedron.command-begin-renderpass command-buffer surface next-image)
  (hedron.command-bind-pipeline command-buffer pipeline)
  (hedron.command-set-surface command-buffer surface)
  (hedron.command-draw command-buffer 3 1 0 0)
  (hedron.command-end-renderpass command-buffer)
  (hedron.command-end command-buffer))

;; TODO: check https://docs.vulkan.org/guide/latest/swapchain_semaphore_reuse.html
;;   for more info on error message

(def draw-frame proc [(fdata FrameObject) (sync SyncObject) pipeline surface
                      (resize (Maybe (Pair U32 U32)))] seq
  (hedron.wait-for-fence fdata.in-flight)

  [let! next-image (hedron.acquire-next-image surface sync.image-available)]
  (match resize
    [[:some extent] seq
      (hedron.resize-window-surface surface extent)]
    [:none seq
      (hedron.reset-fence fdata.in-flight)
      (hedron.reset-command-buffer fdata.command-buffer)
        
      ;; The actual drawing
      (record-command fdata.command-buffer pipeline surface next-image)
        
      (hedron.queue-submit fdata.command-buffer fdata.in-flight sync.image-available sync.render-finished)
      (hedron.queue-present surface sync.render-finished next-image)]))

(def new-winsize proc [(messages (List window.Message))] seq
  (if (u64.= 0 messages.len)
      (Maybe (Pair U32 U32)):none
      (match (elt (u64.- messages.len 1) messages)
        [[:resize x y]
            (Maybe (Pair U32 U32)):some (struct (Pair U32 U32) [._1 x] [._2 y])])))

(def main proc [] seq
  [let! ;; windowing !
        win (window.create-window "My Window" 1080 720)
        surface (hedron.create-window-surface win)

        pipeline (create-graphics-pipeline surface)
        command-pool (hedron.create-command-pool)
        frame-objects (create-frame-objects command-pool) 
        num-images (widen (hedron.num-swapchain-images surface) U64)
        sync-objects (create-sync-objects num-images)]

  (loop [while (bool.not (window.should-close win))]
        [for fence-frame = 0 then (u64.mod (u64.+ fence-frame 1) 2)]
        [for sync-frame = 0 then  (u64.mod (u64.+ sync-frame 1) num-images)]

    (seq 
      [let! events (window.poll-events win)
            winsize (new-winsize events)]
      ;; do nothing with events for now,
      ;; the only event we care abount (resize)

    ;;[let! events (window.poll-events window)]
    ;; (loop [for event in events]
    ;;   (match event
    ;;     [:resize new-width new-height (resize-surface surface new-with new-height)]
    ;;     [_ :unit ])) ;; default/do nothing
      (draw-frame (elt fence-frame frame-objects) (elt sync-frame sync-objects) pipeline surface winsize)))

  (hedron.wait-for-device)

  (each destroy-frame-object frame-objects)
  (each destroy-sync-object sync-objects)
  (hedron.destroy-command-pool command-pool)
  (hedron.destroy-pipeline pipeline)
  (hedron.destroy-window-surface surface)
  (window.destroy-window win))

(main)
