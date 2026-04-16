(vl-load-com)

;;; ===== 輔助函式 =====

;; 建立安全文字標註函式 (改用 entmakex 徹底避免 command 解析空白字元導致的中斷問題)
(defun ACA-draw-text (align pt h rot str / align_h align_v pt3d rad)
  (setq align (strcase align))
  (setq align_h 0 align_v 0)
  (cond
    ((or (= align "L") (= align "")) (setq align_h 0 align_v 0))
    ((= align "C")  (setq align_h 1 align_v 0))
    ((= align "R")  (setq align_h 2 align_v 0))
    ((= align "M")  (setq align_h 4 align_v 0))
    ((= align "TL") (setq align_h 0 align_v 3))
    ((= align "TC") (setq align_h 1 align_v 3))
    ((= align "TR") (setq align_h 2 align_v 3))
    ((= align "ML") (setq align_h 0 align_v 2))
    ((= align "MC") (setq align_h 1 align_v 2))
    ((= align "MR") (setq align_h 2 align_v 2))
    ((= align "BL") (setq align_h 0 align_v 1))
    ((= align "BC") (setq align_h 1 align_v 1))
    ((= align "BR") (setq align_h 2 align_v 1))
  )
  ;; 確保座標為 3D，角度轉換為弧度
  (setq pt3d (list (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0)))
  (setq rad (* (/ rot 180.0) pi))
  (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      (cons 8 (getvar "clayer"))
      '(100 . "AcDbText")
      (cons 10 pt3d)
      (cons 40 h)
      (cons 1 str)
      (cons 50 rad)
      (cons 7 (getvar "textstyle"))
      (cons 72 align_h)
      (cons 11 pt3d)
      (cons 73 align_v)
    )
  )
)

;; 多邊形帶符號面積（正=CCW, 負=CW）
(defun ACA-signed-area (pts / n i sum x1 y1 x2 y2)
  (setq n (length pts) sum 0.0 i 0)
  (repeat n
    (setq x1  (car  (nth i pts))
          y1  (cadr (nth i pts))
          x2  (car  (nth (rem (1+ i) n) pts))
          y2  (cadr (nth (rem (1+ i) n) pts))
          sum (+ sum (- (* x1 y2) (* x2 y1)))
          i   (1+ i)))
  (/ sum 2.0))

;; 2D 叉積 AB × AC（正=C 在 AB 左側）
(defun ACA-cross2d (A B C)
  (- (* (- (car B) (car A)) (- (cadr C) (cadr A)))
     (* (- (cadr B) (cadr A)) (- (car C) (car A)))))

;; 判斷點 P 是否在三角形 ABC 內（含邊界）
(defun ACA-pt-in-tri (P A B C / d1 d2 d3)
  (setq d1 (ACA-cross2d A B P)
        d2 (ACA-cross2d B C P)
        d3 (ACA-cross2d C A P))
  (or (and (>= d1 0) (>= d2 0) (>= d3 0))
      (and (<= d1 0) (<= d2 0) (<= d3 0))))

;; 從清單移除第 i 個元素
(defun ACA-remove-nth (lst i / j result)
  (setq j 0 result nil)
  (foreach item lst
    (if (/= j i) (setq result (append result (list item))))
    (setq j (1+ j)))
  result)

;; 判斷四個連續點 (P0 P1 P2 P3) 構成的四邊形類型
;; 回傳: "rect" / "trap" / nil
;; 同時需確保此四邊形是凸的，否則不能視為單一矩形/梯形
(defun ACA-classify-quad (P0 P1 P2 P3 / a1 a2 a3 a4 c1 c2 c3 c4 convex)
  ;; 凸性檢查：四個叉積同號
  (setq c1 (ACA-cross2d P0 P1 P2)
        c2 (ACA-cross2d P1 P2 P3)
        c3 (ACA-cross2d P2 P3 P0)
        c4 (ACA-cross2d P3 P0 P1))
  (setq convex (or (and (>= c1 -1e-6) (>= c2 -1e-6) (>= c3 -1e-6) (>= c4 -1e-6))
                   (and (<= c1 1e-6) (<= c2 1e-6) (<= c3 1e-6) (<= c4 1e-6))))
  (if (not convex)
    nil
    (progn
      (setq a1 (angle P0 P1)
            a2 (angle P1 P2)
            a3 (angle P2 P3)
            a4 (angle P3 P0))
      (cond
        ;; 矩形：兩組對邊平行 + 相鄰邊垂直
        ;; 平行判斷用 sin(差角) ? 0 (避免 rem 在浮點邊界的不穩定)
        ;; 垂直判斷用 cos(夾角) ? 0
        ((and (< (abs (sin (- a1 a3))) 0.02)
              (< (abs (sin (- a2 a4))) 0.02)
              (< (abs (cos (- a1 a2))) 0.05))
         "rect")
        ;; 梯形：至少一組對邊平行
        ((or (< (abs (sin (- a1 a3))) 0.02)
             (< (abs (sin (- a2 a4))) 0.02))
         "trap")
        (t nil)))))

;; 檢查線段 (A B) 是否會與多邊形邊發生交叉(端點除外)
;; 用於確認對角切割線是否合法(不穿越其他邊)
(defun ACA-seg-crosses-poly (A B poly-pts / n k q1 q2 found d1 d2 d3 d4)
  (setq n (length poly-pts) k 0 found nil)
  (while (and (< k n) (not found))
    (setq q1 (nth k poly-pts)
          q2 (nth (rem (1+ k) n) poly-pts))
    ;; 若 (q1,q2) 與 (A,B) 共用端點，跳過
    (if (not (or (equal q1 A 1e-6) (equal q1 B 1e-6)
                 (equal q2 A 1e-6) (equal q2 B 1e-6)))
      (progn
        (setq d1 (ACA-cross2d A B q1)
              d2 (ACA-cross2d A B q2)
              d3 (ACA-cross2d q1 q2 A)
              d4 (ACA-cross2d q1 q2 B))
        ;; 嚴格相交：兩線段彼此跨越
        (if (and (< (* d1 d2) -1e-12) (< (* d3 d4) -1e-12))
          (setq found t))))
    (setq k (1+ k)))
  found)

;; 耳切法三角剖分（保證對角線在多邊形內，適用凸/凹多邊形）
(defun ACA-ear-clip (pts / n ccw remaining result j found-ear A B C has-inside)
  (setq ccw       (> (ACA-signed-area pts) 0)
        remaining pts
        result    nil)
  (while (> (length remaining) 3)
    (setq n (length remaining) found-ear nil j 0)
    (while (and (< j n) (not found-ear))
      (setq A          (nth (rem (+ j (1- n)) n) remaining)
            B          (nth j remaining)
            C          (nth (rem (1+ j) n) remaining)
            has-inside nil)
      (if (if ccw (> (ACA-cross2d A B C) 1e-8)
                  (< (ACA-cross2d A B C) -1e-8))
        (progn
          (foreach P remaining
            (if (not (or (equal P A 1e-6) (equal P B 1e-6) (equal P C 1e-6)))
              (if (ACA-pt-in-tri P A B C) (setq has-inside t))))
          (if (not has-inside)
            (setq found-ear t
                  result    (append result (list (list A B C)))
                  remaining (ACA-remove-nth remaining j)))))
      (if (not found-ear) (setq j (1+ j))))
    (if (not found-ear)
      (setq remaining (list (car remaining) (cadr remaining) (caddr remaining)))))
  (append result (list (list (car remaining) (cadr remaining) (caddr remaining)))))

;; 判斷邊 (p1,p2) 是否為多邊形原始邊
(defun ACA-is-poly-edge (p1 p2 poly-pts / n k q1 q2 found)
  (setq n (length poly-pts) k 0 found nil)
  (while (and (< k n) (not found))
    (setq q1 (nth k poly-pts)
          q2 (nth (rem (1+ k) n) poly-pts))
    (if (or (and (equal p1 q1 1e-6) (equal p2 q2 1e-6))
            (and (equal p1 q2 1e-6) (equal p2 q1 1e-6)))
      (setq found t))
    (setq k (1+ k)))
  found)

;; 取得弧段圓心座標
(defun ACA-arc-center (pt_a pt_b bv / d theta r dist_c chord_ang perp_ang dir_to_apex mx my)
  (setq d         (distance pt_a pt_b)
        theta     (* 4.0 (atan bv))
        r         (/ d (* 2.0 (sin (/ (abs theta) 2.0))))
        dist_c    (abs (* r (cos (/ (abs theta) 2.0)))) ; 強制為正，方向由 perp_ang 控制
        chord_ang (angle pt_a pt_b)
        mx        (/ (+ (car pt_a) (car pt_b)) 2.0)
        my        (/ (+ (cadr pt_a) (cadr pt_b)) 2.0))
  ;; 1. 取得從弦中點指向弧頂(凸出側)的正確方向
  (setq dir_to_apex (if (> bv 0) (- chord_ang (/ pi 2.0)) (+ chord_ang (/ pi 2.0))))
  ;; 2. 判斷主弧/劣弧，決定圓心方向
  ;;    - 劣弧 (<=180度): 圓心在弧頂的另一側
  ;;    - 主弧 (>180度): 圓心與弧頂在同一側
  (if (> (abs theta) pi) ; 如果是主弧 (Major Arc, > 180度)
    (setq perp_ang dir_to_apex)        ; 圓心與弧頂在同一側
    (setq perp_ang (+ dir_to_apex pi)) ; 圓心在弧頂的另一側 (Minor Arc)
  )
  (polar (list mx my 0.0) perp_ang dist_c))

;; 從點 P 作垂足到直線 AB
(defun ACA-perp-foot (P A B / abx aby apx apy denom tv)
  ;; 確保點是 3D 點，避免 2D 點與 3D 點運算錯誤
  (setq A (trans A 0 1) B (trans B 0 1) P (trans P 0 1))
  (setq abx   (- (car B) (car A))
        aby   (- (cadr B) (cadr A))
        apx   (- (car P) (car A))
        apy   (- (cadr P) (cadr A))
        denom (+ (* abx abx) (* aby aby)))
  (if (< (abs denom) 1e-10)
    (list (car A) (cadr A) 0.0)
    (progn
      (setq tv (/ (+ (* abx apx) (* aby apy)) denom))
      (list (+ (car A) (* tv abx)) (+ (cadr A) (* tv aby)) 0.0))))

;; 三角形重心
(defun ACA-centroid3 (p0 p1 p2)
  (list (/ (+ (car p0) (car p1) (car p2)) 3.0)
        (/ (+ (cadr p0) (cadr p1) (cadr p2)) 3.0)
        0.0))

;; 兩點中點
(defun ACA-midpt (p1 p2)
  (list (/ (+ (car p1) (car p2)) 2.0)
        (/ (+ (cadr p1) (cadr p2)) 2.0)
        0.0))

;; 弧頂點（用於扇形/弓形標籤定位）
(defun ACA-arc-midpt (pt_a pt_b bv / d theta r sagitta chord_ang arc_dir mx my)
  (setq d         (distance pt_a pt_b)
        theta     (* 4.0 (atan bv))
        r         (/ d (* 2.0 (sin (/ (abs theta) 2.0))))
        sagitta   (* r (- 1.0 (cos (/ (abs theta) 2.0))))
        chord_ang (angle pt_a pt_b)
        arc_dir   (if (> bv 0) (- chord_ang (/ pi 2.0)) (+ chord_ang (/ pi 2.0)))
        mx        (/ (+ (car pt_a) (car pt_b)) 2.0)
        my        (/ (+ (cadr pt_a) (cadr pt_b)) 2.0))
  (polar (list mx my 0.0) arc_dir sagitta))

;; 扇形幾何重心 (精確微積分公式)
(defun ACA-sector-centroid (ctr arc_midpt r theta / alpha dist_c ang)
  (setq alpha (/ (abs theta) 2.0))
  (if (< alpha 1e-6)
    arc_midpt
    (progn
      (setq dist_c (/ (* 2.0 r (sin alpha)) (* 3.0 alpha)))
      (setq ang (angle ctr arc_midpt))
      (polar (list (car ctr) (cadr ctr) 0.0) ang dist_c)
    )
  )
)

;; 弓形幾何重心 (精確微積分公式)
(defun ACA-segment-centroid (ctr arc_midpt r theta / alpha den num dist_c ang)
  (setq alpha (/ (abs theta) 2.0))
  (setq den (- (abs theta) (sin (abs theta))))
  (if (< (abs den) 1e-6)
    arc_midpt
    (progn
      (setq num (* 4.0 r (expt (sin alpha) 3)))
      (setq dist_c (/ num (* 3.0 den)))
      (setq ang (angle ctr arc_midpt))
      (polar (list (car ctr) (cadr ctr) 0.0) ang dist_c)
    )
  )
)

;; 多邊形幾何重心 (依頂點計算，避開 ActiveX 不支援 Polyline 重心的問題)
(defun ACA-poly-centroid (pts / n i area sum_x sum_y x0 y0 x1 y1 cross)
  (setq n (length pts) area 0.0 sum_x 0.0 sum_y 0.0 i 0)
  (repeat n
    (setq x0 (car (nth i pts))
          y0 (cadr (nth i pts))
          x1 (car (nth (rem (1+ i) n) pts))
          y1 (cadr (nth (rem (1+ i) n) pts))
          cross (- (* x0 y1) (* x1 y0))
          area (+ area cross)
          sum_x (+ sum_x (* (+ x0 x1) cross))
          sum_y (+ sum_y (* (+ y0 y1) cross))
          i (1+ i)))
  (setq area (/ area 2.0))
  (if (equal (abs area) 0.0 1e-6)
    (list (car (nth 0 pts)) (cadr (nth 0 pts)) 0.0) ; 防呆處理
    (list (/ sum_x (* 6.0 area)) (/ sum_y (* 6.0 area)) 0.0)))

;; 繪製圓心十字標記
(defun ACA-draw-center-mark (pt size / p1 p2 p3 p4)
  (setq p1 (list (- (car pt) size) (cadr pt) 0.0)
        p2 (list (+ (car pt) size) (cadr pt) 0.0)
        p3 (list (car pt) (- (cadr pt) size) 0.0)
        p4 (list (car pt) (+ (cadr pt) size) 0.0))
  (entmakex (list '(0 . "LINE") (cons 8 (getvar "clayer")) (cons 10 p1) (cons 11 p2)))
  (entmakex (list '(0 . "LINE") (cons 8 (getvar "clayer")) (cons 10 p3) (cons 11 p4)))
)

;; 繪製垂直符號 (直角記號)
(defun ACA-draw-perp-symbol (foot apex bpt1 bpt2 size / ang_base ang_height p2 p3 p4)
  ;; 選擇底線上距離垂足較遠的端點作為符號繪製方向，避免畫出線外
  (if (> (distance foot bpt1) (distance foot bpt2))
    (setq ang_base (angle foot bpt1))
    (setq ang_base (angle foot bpt2))
  )
  (setq ang_height (angle foot apex))
  (setq foot (list (car foot) (cadr foot) 0.0)
        p2 (polar foot ang_base size)
        p4 (polar foot ang_height size)
        p3 (polar p2 ang_height size))
  (entmakex (list '(0 . "LINE") (cons 8 (getvar "clayer")) (cons 10 p2) (cons 11 p3)))
  (entmakex (list '(0 . "LINE") (cons 8 (getvar "clayer")) (cons 10 p3) (cons 11 p4)))
)

;; 偵測多邊形是否有邊重疊 (任兩條非相鄰邊共線且有區段重疊)
;; 回傳 t 表示有問題, nil 表示正常
(defun ACA-has-overlapping-edges (pts bvs / n j k pA pB pC pD
          angAB angCD dAB lenAB tC tD tCp tDp overlap)
  (setq n (length pts) overlap nil)
  (setq j 0)
  (while (and (< j n) (not overlap))
    ;; 只檢查直線段 (bulge = 0)
    (if (equal (nth j bvs) 0.0 1e-10)
      (progn
        (setq pA (nth j pts)
              pB (nth (rem (1+ j) n) pts)
              angAB (angle pA pB)
              lenAB (distance pA pB))
        (if (> lenAB 1e-10)
          (progn
            (setq k (+ j 2))
            (while (and (< k (+ n j -1)) (not overlap))
              (setq k_mod (rem k n))
              (if (equal (nth k_mod bvs) 0.0 1e-10)
                (progn
                  (setq pC (nth k_mod pts)
                        pD (nth (rem (1+ k_mod) n) pts)
                        angCD (angle pC pD))
                  ;; 兩邊平行 (同方向或反方向)
                  (if (or (equal (rem angAB pi) (rem angCD pi) 0.005)
                          (equal (abs (- angAB angCD)) pi 0.005))
                    (progn
                      ;; 計算 C、D 到直線 AB 的垂直距離
                      (setq foot_C (ACA-perp-foot pC pA pB)
                            dist_C (distance pC foot_C)
                            foot_D (ACA-perp-foot pD pA pB)
                            dist_D (distance pD foot_D))
                      ;; 若兩端點都在直線 AB 上 (垂距 < 容差)
                      (if (and (< dist_C (* lenAB 1e-4))
                               (< dist_D (* lenAB 1e-4)))
                        (progn
                          ;; 投影 C、D 到 AB 方向的參數
                          (setq tC (/ (+ (* (- (car pC) (car pA)) (- (car pB) (car pA)))
                                         (* (- (cadr pC) (cadr pA)) (- (cadr pB) (cadr pA))))
                                      (* lenAB lenAB))
                                tD (/ (+ (* (- (car pD) (car pA)) (- (car pB) (car pA)))
                                         (* (- (cadr pD) (cadr pA)) (- (cadr pB) (cadr pA))))
                                      (* lenAB lenAB)))
                          (setq tCp (min tC tD) tDp (max tC tD))
                          ;; 檢查投影區間 [tCp, tDp] 與 [0, 1] 是否有重疊
                          (if (and (< tCp (- 1.0 1e-6)) (> tDp 1e-6))
                            (setq overlap t))))))
                ))
              (setq k (1+ k)))))))
    (setq j (1+ j)))
  overlap)

;; 偵測多邊形是否自我交結 (線段互相跨越，會造成面積計算異常)
;; 弧段先離散化為折線再檢查，可捕捉「邊線交叉」或「連續弧段繞行自交」等情境
;; 回傳 t 表示有問題, nil 表示正常
(defun ACA-has-self-intersection (pts bvs / n j k segs pA pB bv chord
                                  theta r ctr chord_ang start_ang step sub_n si
                                  sA sB tA tB d1 d2 d3 d4 found)
  (setq n (length pts) segs nil j 0)
  ;; 步驟 1：將多邊形的每條邊離散化為「小線段列表」segs
  ;; 直線邊 → 1 段；弧邊 → 切成 16 段
  (while (< j n)
    (setq pA (nth j pts)
          pB (nth (rem (1+ j) n) pts)
          bv (nth j bvs))
    (if (equal bv 0.0 1e-10)
      ;; 直線段
      (setq segs (append segs (list (list pA pB))))
      ;; 弧段：離散化
      (progn
        (setq theta (* 4.0 (atan bv))
              chord (distance pA pB)
              r     (/ (* chord (+ 1.0 (* bv bv))) (* 4.0 (abs bv)))
              ctr   (ACA-arc-center pA pB bv)
              start_ang (angle ctr pA)
              sub_n 16
              step (/ theta sub_n)
              si 0)
        (setq sA pA)
        (while (< si sub_n)
          (setq si (1+ si))
          (if (= si sub_n)
            (setq sB pB)
            (setq sB (polar ctr (+ start_ang (* step si)) r)))
          (setq segs (append segs (list (list sA sB))))
          (setq sA sB))))
    (setq j (1+ j)))
  ;; 步驟 2：雙迴圈檢查任兩條非相鄰小線段是否嚴格跨越
  (setq n (length segs) j 0 found nil)
  (while (and (< j (1- n)) (not found))
    (setq sA (car (nth j segs)) sB (cadr (nth j segs)))
    (setq k (+ j 2))
    (while (and (< k n) (not found))
      ;; 跳過環狀相鄰 (最後一段與第一段)
      (if (not (and (= j 0) (= k (1- n))))
        (progn
          (setq tA (car (nth k segs)) tB (cadr (nth k segs)))
          ;; 檢查端點是否重合，若是則不算交叉(正常拐角)
          ;; 容差 1e-4 以吸收弧離散化的浮點精度差異
          (if (not (or (< (distance sA tA) 1e-4) (< (distance sA tB) 1e-4)
                       (< (distance sB tA) 1e-4) (< (distance sB tB) 1e-4)))
            (progn
              (setq d1 (ACA-cross2d sA sB tA)
                    d2 (ACA-cross2d sA sB tB)
                    d3 (ACA-cross2d tA tB sA)
                    d4 (ACA-cross2d tA tB sB))
              ;; 嚴格相交：兩線段彼此跨越
              (if (and (< (* d1 d2) -1e-12) (< (* d3 d4) -1e-12))
                (setq found t))))))
      (setq k (1+ k)))
    (setq j (1+ j)))
  found)

;;; ===== 主指令 =====

(defun c:AutoCalArea (/ ss i ent obj minp maxp p1 p3 cp area total_area
                        table_pt col_w1 col_w2 row_pt txt_h sc
                        pts_list bv_list k pt_a pt_b bv
                        n is_arc ang disp_ang dist midp
                        sub_shapes diag_lines fan_cnt bow_cnt tri_cnt poly_ccw arc_sign
                        w h a1 a2 a3 a4 b1 b2 h_trap
                        theta theta_deg r r_m seg_a seg_a_m2 chord ctr foot
                        base_len base_len_m height_len_m tri_result
                        base_pt1 base_pt2 apex tri_ctr lbl sctr
                        poly_pts triangles tri t_area d01 d12 d20
                        tA tB tC shape aux_type aux_pts sgn
                        tri_foot_s tri_base_m_s tri_h_m_s
                        tri_area_s tri_lbl_s tri_ctr_s
                        hbm hhm arc_midpt old_osmode old_err
                        lbl_str val_str sum_total first_entry sum_line arc_count arc_k
                        txt_ang offset_ang perp_ang
                        lay_name lay_color lay_ltype def layer_defs
                        ctr_from_arc area_m2 lbl-sign skip_edges
                        i_shape num_shapes lbl_lines val_lines cur_lbl cur_val first_val
                        b1_pts b2_pts foot1 foot2 d_base
                        rect_cnt trap_cnt run_ear_clip is_rect_part is_trap_part
                        w_m h_m rect_area b1_m b2_m trap_area trap_area_m rctr
                        found_quad q_idx q_n qP0 qP1 qP2 qP3 quad_type accept_pass
                        idx_rm1 idx_rm2 h_trap_m
                        min_dist need_offset
                        rect_edges bot_edge left_edge e
                        rect_used_edges rap rk re edge_skip
                        rctr_m e0 e1 e2 e3 edge_A edge_B
                        base_idx base_pt next_pt prev_pt chord_edges ce
                        opp_pt edge1_skip edge2_skip
                        chord_a chord_b foot_a foot_b chord_len chord_count
                        ref_a ref_b ref_len has_major_sector)
  (setvar "cmdecho" 0)
  (command "_undo" "_begin")
  
  ;; 儲存系統變數並設定錯誤處理 (防止 OSNAP 干擾點選位置)
  (setq old_osmode (getvar "osmode"))
  (setq old_err *error*)
  (defun *error* (msg)
    (if old_osmode (setvar "osmode" old_osmode))
    (setvar "cmdecho" 1)
    (command-s "_undo" "_end")
    (setq *error* old_err)
    (princ (strcat "\n*** [ACA 錯誤] " (if msg msg "未知錯誤") " ***"))
    (princ "\n*** 處理已中斷 ***")
    (princ))
    
  (setvar "osmode" 0) ;; 關閉物件鎖點以防止標註偏移或跳點

  ;; --- 1. 參數設定 (透過 ACAO / ACAL 指令設定) ---
  (if (not *AAF_txt_h*) (setq *AAF_txt_h* 15.0))
  (if (not *AAF_sc*)    (setq *AAF_sc* 100.0))
  (if (not *AAF_main_space*) (setq *AAF_main_space* 2.0))
  (if (not *AAF_sub_space*)  (setq *AAF_sub_space* 1.8))
  (if (not *AAF_sym_scale*)  (setq *AAF_sym_scale* 0.8))
  (if (not *AAF_pfx_sector*)  (setq *AAF_pfx_sector* "S"))
  (if (not *AAF_pfx_segment*) (setq *AAF_pfx_segment* "G"))
  (if (not *AAF_pfx_rect*)    (setq *AAF_pfx_rect* "J"))
  (if (not *AAF_pfx_tri*)     (setq *AAF_pfx_tri* "A"))
  (if (not *AAF_pfx_trap*)    (setq *AAF_pfx_trap* "T"))
  (setq txt_h *AAF_txt_h* sc *AAF_sc*)

  ;; --- 2. 圖層設定 ---
  ;; 載入所需線型
  (if (null (tblsearch "LTYPE" "DASHED"))
    (command "-linetype" "load" "DASHED" "" "")
  )
  ;; 定義圖層屬性: (圖層名稱 顏色索引 線型名稱)
  ;; 註: AREA-數值 顏色依使用者提供的索引號碼 8 (灰色) 設定。
  (setq layer_defs
    '(("AREA-文字" 3 nil)
      ("AREA-數值" 6 nil) ; 修正為洋紅 (索引顏色 6)
      ("AREA-編號" 3 nil)
      ("AREA-切割線" 8 nil)
      ("AREA-輔助線" 9 "DASHED")
      ("AREA-符號" 9 nil)
    )
  )
  (foreach def layer_defs
    (setq lay_name (car def)
          lay_color (cadr def)
          lay_ltype (caddr def))
    ;; 建立圖層並設定屬性
    (command "-layer" "m" lay_name "c" lay_color lay_name "lt" (if lay_ltype lay_ltype "Continuous") lay_name "")
  )

  ;; --- 3. 選取封閉 LWPOLYLINE ---
  (setvar "osmode" old_osmode) ;; 開啟鎖點以利使用者選取
  (setq ss (ssget '((0 . "LWPOLYLINE") (70 . 1))))
  (if ss
    (progn
      (setq table_pt (getpoint "\n點選計算表起始位置: "))
      (if table_pt
        (progn
          (setvar "osmode" 0) ;; 重新關閉鎖點進行自動繪製
          (setq col_w1 (* txt_h 8.0) col_w2 (* txt_h 50.0) total_area 0 i 0)
          (setvar "clayer" "AREA-文字")
          (ACA-draw-text "l" table_pt txt_h 0.0 "編號")
          (ACA-draw-text "l" (polar table_pt 0 col_w1) txt_h 0.0 "計算式 (ACA)")
          (ACA-draw-text "l" (polar table_pt 0 (+ col_w1 col_w2)) txt_h 0.0 "面積 (m2)")
          (setq row_pt (polar table_pt (/ pi -2) (* txt_h 3.0)))

          (repeat (sslength ss)
            (princ (strcat "\n\n[DEBUG] --- 開始處理第 " (itoa (1+ i)) " 個圖形 ---"))
            (setq ent       (ssname ss i)
                  obj       (vlax-ename->vla-object ent)
                  area      (/ (vla-get-area obj) (* sc sc))
                  total_area (+ total_area area)
                  ;; ---- 強制清空所有殘留變數，徹底避免干擾下一個圖形的邏輯判斷 ----
                  sub_shapes nil diag_lines nil fan_cnt 0 bow_cnt 0 tri_cnt 0 rect_cnt 0 trap_cnt 0
                  pts_list nil bv_list nil poly_pts nil triangles nil
                  is_arc nil poly_ccw nil arc_sign nil
                  w nil h nil a1 nil a2 nil a3 nil a4 nil b1 nil b2 nil h_trap nil
                  base_len nil base_len_m nil height_len_m nil tri_result nil
                  apex nil foot nil base_pt1 nil base_pt2 nil tri_ctr nil lbl nil sctr nil
                  theta nil theta_deg nil r nil r_m nil chord nil ctr nil
                  seg_a nil seg_a_m2 nil tA nil tB nil tC nil t_area nil
                  d01 nil d12 nil d20 nil shape nil aux_type nil aux_pts nil sgn nil
                  tri_foot_s nil tri_base_m_s nil tri_h_m_s nil tri_area_s nil tri_lbl_s nil tri_ctr_s nil
                  hbm nil hhm nil arc_midpt nil lbl_str nil val_str nil sum_total nil first_entry nil sum_line nil
                  arc_count nil arc_k nil txt_ang nil offset_ang nil perp_ang nil midp nil ang nil disp_ang nil ctr_from_arc nil
                  lbl-sign nil area_m2 nil skip_edges nil
                  i_shape nil num_shapes nil lbl_lines nil val_lines nil cur_lbl nil cur_val nil first_val nil
                  b1_pts nil b2_pts nil foot1 nil foot2 nil d_base nil
                  run_ear_clip nil is_rect_part nil is_trap_part nil w_m nil h_m nil rect_area nil b1_m nil b2_m nil trap_area nil trap_area_m nil rctr nil)

            ;; A. 取得頂點清單與 bulge 清單
            (vla-getboundingbox obj 'minp 'maxp)
            (setq p1 (vlax-safearray->list minp)
                  p3 (vlax-safearray->list maxp)
                  w  (/ (abs (- (car p3) (car p1))) sc)
                  h  (/ (abs (- (cadr p3) (cadr p1))) sc))
            (setq k 0 pts_list nil bv_list nil)
            (repeat (fix (vlax-curve-getEndParam obj))
              (setq pts_list (append pts_list (list (vlax-curve-getPointAtParam obj k))))
              (setq bv_list  (append bv_list  (list (vla-GetBulge obj k))))
              (setq k (1+ k)))
            (setq n (length pts_list))
            (setq is_arc nil)
            (foreach bv bv_list (if (/= bv 0.0) (setq is_arc t)))
            (princ (strcat "\n[DEBUG] 頂點擷取完成: 邊數 n=" (itoa n) "，包含弧線: " (if is_arc "是" "否")))

            ;; 防蠢機制：偵測多邊形是否有線段重疊或自我交結
            (cond
              ((ACA-has-overlapping-edges pts_list bv_list)
               (princ (strcat "\n*** [ACA 警告] 第 " (itoa (1+ i)) " 個圖形偵測到邊線重疊，跳過此圖形！***"))
               (princ "\n    請檢查多邊形是否有線段來回重疊。")
               (setq i (1+ i)))
              ((ACA-has-self-intersection pts_list bv_list)
               (princ (strcat "\n*** [ACA 警告] 第 " (itoa (1+ i)) " 個圖形偵測到線段自我交結 (面積重疊)，跳過此圖形！***"))
               (princ "\n    常見原因：邊線彼此交叉，或連續內凹/外凸弧段繞行造成圖形自交。")
               (setq i (1+ i)))
              (t
               (progn ; 正常處理開始

            ;; B. 形狀分類
            ;; sub_shapes 結構: (list 標籤 公式字串 面積m2 輔助線類型 輔助線點清單 標籤中心 正負號)
            (cond

              ;; ---- 矩形 (幾何判斷：兩對邊平行 + 相鄰邊垂直，不受旋轉影響) ----
              ((and (= n 4) (not is_arc)
                    (progn
                      (setq a1 (angle (nth 0 pts_list) (nth 1 pts_list))
                            a3 (angle (nth 2 pts_list) (nth 3 pts_list))
                            a2 (angle (nth 1 pts_list) (nth 2 pts_list))
                            a4 (angle (nth 3 pts_list) (nth 0 pts_list)))
                      (and (< (abs (sin (- a1 a3))) 0.02)
                           (< (abs (sin (- a2 a4))) 0.02)
                           (< (abs (cos (- a1 a2))) 0.05))))
               (princ "\n[DEBUG] 形狀分類判定為: 矩形")
               (setq skip_edges t)
               ;; w、h 用實際邊長 (非 bounding box),處理傾斜矩形
               (setq w (/ (distance (nth 0 pts_list) (nth 1 pts_list)) sc)
                     h (/ (distance (nth 1 pts_list) (nth 2 pts_list)) sc))
               (setq sub_shapes
                 (list (list "" ; 空標籤，因為是單一計算式
                             (strcat (rtos w 2 2) " × " (rtos h 2 2)
                                     " = " (rtos area 2 2) "m?")
                             area "rect" (list (nth 0 pts_list) (nth 1 pts_list) (nth 2 pts_list) (nth 3 pts_list)) nil 1))))

              ;; ---- 梯形 ----
              ((and (= n 4) (not is_arc)
                    (progn
                      (setq a1 (angle (nth 0 pts_list) (nth 1 pts_list))
                            a3 (angle (nth 2 pts_list) (nth 3 pts_list))
                            a2 (angle (nth 1 pts_list) (nth 2 pts_list))
                            a4 (angle (nth 3 pts_list) (nth 0 pts_list)))
                      (or (< (abs (sin (- a1 a3))) 0.02)
                          (< (abs (sin (- a2 a4))) 0.02))))
               (if (< (abs (sin (- a1 a3))) 0.02)
                 (setq b1_pts (list (nth 0 pts_list) (nth 1 pts_list)) b2_pts (list (nth 2 pts_list) (nth 3 pts_list)))
                 (setq b1_pts (list (nth 1 pts_list) (nth 2 pts_list)) b2_pts (list (nth 3 pts_list) (nth 0 pts_list))))
               (setq b1 (/ (distance (car b1_pts) (cadr b1_pts)) sc) b2 (/ (distance (car b2_pts) (cadr b2_pts)) sc))
               (setq h_trap (/ (* area 2.0) (+ b1 b2))) ; 修正：補回遺失的梯形高計算公式
               (princ "\n[DEBUG] 形狀分類判定為: 梯形")
               (setq skip_edges t)
               (setq sub_shapes
                 (list (list "" ; 空標籤，因為是單一計算式
                             (strcat "(" (rtos b1 2 2) "+" (rtos b2 2 2)
                                     ") × " (rtos h_trap 2 2) " / 2 = " (rtos area 2 2) "m?")
                             area "trap" (list (car b1_pts) (cadr b1_pts) (car b2_pts) (cadr b2_pts)) nil 1))))

              ;; ---- 三角形 (3點) ----
              ((and (= n 3) (not is_arc))
               (setq d01 (distance (nth 0 pts_list) (nth 1 pts_list))
                     d12 (distance (nth 1 pts_list) (nth 2 pts_list))
                    d20 (distance (nth 2 pts_list) (nth 0 pts_list))
                    tA  (nth 0 pts_list)
                    tB  (nth 1 pts_list)
                    tC  (nth 2 pts_list))
               (cond
                ;; 直角在 C (斜邊 d01)
                ((< (abs (- (+ (* d12 d12) (* d20 d20)) (* d01 d01))) (* 1e-3 d12 d20))
                 (setq base_pt1 tB base_pt2 tC apex tA base_len d12))
                ;; 直角在 A (斜邊 d12)
                ((< (abs (- (+ (* d01 d01) (* d20 d20)) (* d12 d12))) (* 1e-3 d01 d20))
                 (setq base_pt1 tA base_pt2 tB apex tC base_len d01))
                ;; 直角在 B (斜邊 d20)
                ((< (abs (- (+ (* d01 d01) (* d12 d12)) (* d20 d20))) (* 1e-3 d01 d12))
                 (setq base_pt1 tA base_pt2 tB apex tC base_len d01))
                ;; 預設找最長邊當底
                ((and (>= d01 d12) (>= d01 d20))
                 (setq base_pt1 tA base_pt2 tB apex tC base_len d01))
                ((and (>= d12 d01) (>= d12 d20))
                 (setq base_pt1 tB base_pt2 tC apex tA base_len d12))
                 (t
                 (setq base_pt1 tC base_pt2 tA apex tB base_len d20)))
               (setq foot         (ACA-perp-foot apex base_pt1 base_pt2)
                     base_len_m   (/ base_len sc)
                     height_len_m (/ (distance apex foot) sc)
                     tri_result   (* 0.5 base_len_m height_len_m)
                     tri_ctr      (ACA-centroid3 (nth 0 pts_list)
                                                 (nth 1 pts_list)
                                                 (nth 2 pts_list)))
               (princ "\n[DEBUG] 形狀分類判定為: 三角形")
               (setq skip_edges t)
               (setq sub_shapes
                 (list (list "" ; 空標籤，因為是單一計算式
                             (strcat (rtos base_len_m 2 2)
                                     " × " (rtos height_len_m 2 2)
                                     " / 2 = " (rtos tri_result 2 2) "m?")
                             tri_result "height" (list apex foot base_pt1 base_pt2) tri_ctr 1))))

              ;; ---- 完整扇形 (3點, 1弧) ----
              ((and (= n 3) is_arc
                    (progn
                      (setq arc_count 0 k 0 arc_k -1)
                      (while (< k n)
                        (if (/= (nth k bv_list) 0.0)
                          (setq arc_count (1+ arc_count) arc_k k))
                        (setq k (1+ k)))
                      (= arc_count 1))
                    (progn
                      (setq pt_a (nth arc_k pts_list)
                            pt_b (nth (rem (1+ arc_k) n) pts_list)
                            apex (nth (rem (+ arc_k 2) n) pts_list) ; The vertex not part of the arc
                            bv   (nth arc_k bv_list)
                            chord (distance pt_a pt_b)
                            r     (/ (* chord (+ 1.0 (* bv bv))) (* 4.0 (abs bv)))
                            r_m   (/ r sc)
                            ctr_from_arc (ACA-arc-center pt_a pt_b bv))
                      ;; 條件：頂點必須與弧段圓心重合 (容差 1e-4)，代表是標準扇形(含直角)
                      (equal apex ctr_from_arc 1e-4)))
               (princ "\n[DEBUG] 形狀分類判定為: 完整扇形")
               (setq skip_edges t)
               (setq theta     (* 4.0 (atan bv))
                     theta_deg (* (/ (abs theta) pi) 180.0)
                     sctr     (ACA-sector-centroid apex (ACA-arc-midpt pt_a pt_b bv) r theta)
                     area_m2  (/ (vla-get-area obj) (* sc sc)))
               (setq sub_shapes
                 (list (list "" ; 空標籤，單一圖形不編 S 號
                             (strcat "(" (rtos theta_deg 2 1) "°/360)×π×(" (rtos r_m 2 2) ")?=" (rtos area_m2 2 2) "m?")
                             area_m2 "sector"
                             ;; aux_pts for sector: (pt_a ctr pt_b r_m theta_deg arc_midpt)
                             (list pt_a apex pt_b r_m theta_deg (ACA-arc-midpt pt_a pt_b bv))
                             sctr 1))))

              ;; ---- 含弧段 或 多邊形 ----
              (t
               (princ "\n[DEBUG] 形狀分類判定為: 一般多邊形 / 複合圖形")
               ;; 判斷多邊形方向（決定弓/扇形面積正負號）
               (setq poly_ccw (> (ACA-signed-area pts_list) 0))
               ;; has_major_sector：存在 |θ|>180° 主扇形 → 後續耳切/矩形抽取的子形狀符號需反轉
               ;; (因為主扇形 = 整塊加進來，其餘部分實際上是「從扇形中扣除」的)
               (setq has_major_sector nil)

               ;; 步驟1：掃描弧段 → 扇形/弓形，建立折線頂點 poly_pts
               (setq poly_pts nil k 0)
               (repeat n
                 (setq pt_a (nth k pts_list)
                       pt_b (nth (rem (1+ k) n) pts_list)
                       bv   (nth k bv_list))
                 (setq poly_pts (append poly_pts (list pt_a)))
                 (if (/= bv 0.0)
                   (progn
                     (setq theta     (* 4.0 (atan bv))
                           theta_deg (* (/ (abs theta) pi) 180.0)
                           chord     (distance pt_a pt_b)
                           r         (/ (* chord (+ 1.0 (* bv bv))) (* 4.0 (abs bv)))
                           r_m       (/ r sc)
                           ctr       (ACA-arc-center pt_a pt_b bv)
                           ;; bulge 與多邊形方向同號 → 弧往外 → 正面積
                           arc_sign  (if (eq (> bv 0) poly_ccw) 1 -1))
                     (if (> (abs theta) pi)
                       ;; ---- 扇形（|θ| > 180°）---- 扇形 + 三角形(pt_a, ctr, pt_b)
                       ;; 幾何關係：扇形涵蓋 |θ| > 180° 的區域
                       ;; 分解為：扇形整塊 (+ 一定) 扣除 (pt_a, ctr, pt_b) 三角形 (- 因被扇形包含)
                       ;; 此分解獨立於多邊形繞行方向與 bulge 符號
                       (progn
                         (setq has_major_sector t) ; 標記存在主扇形
                         (setq fan_cnt  (1+ fan_cnt)
                               lbl      (strcat *AAF_pfx_sector* (itoa fan_cnt))
                               seg_a_m2 (* (/ theta_deg 360.0) pi r_m r_m)
                               sctr     (ACA-sector-centroid ctr (ACA-arc-midpt pt_a pt_b bv) r theta))
                         ;; 扇形 sub_shape：永遠為正號 (主面積)
                         (setq sub_shapes (append sub_shapes
                           (list (list lbl
                             (strcat lbl ": θ/360×π×r?  r=" (rtos r_m 2 2)
                                         "m  θ=" (rtos theta_deg 2 1)
                                         "° (" (rtos theta_deg 2 1)
                                         "/360)×π×(" (rtos r_m 2 2)
                                         ")?=" (rtos seg_a_m2 2 2) "m?")
                             seg_a_m2 "sector"
                             (list pt_a ctr pt_b r_m theta_deg (ACA-arc-midpt pt_a pt_b bv))
                             sctr 1))))
                         ;; 扇形的弦線（pt_a → pt_b）加入輔助線
                         (setq diag_lines (append diag_lines (list (list pt_a pt_b))))
                         ;; 三角形 (pt_a, ctr, pt_b) — 被扇形包含，需從扇形中扣除
                         ;; 三角形 (pt_a, ctr, pt_b) — 在扇形區域之外(補角那側)
                         ;; 與扇形一起構成完整的大扇形多邊形區域，因此符號 = +1
                         (setq tri_foot_s   (ACA-perp-foot ctr pt_a pt_b)
                               tri_base_m_s (/ (distance pt_a pt_b) sc)
                               tri_h_m_s    (/ (distance ctr tri_foot_s) sc)
                               tri_area_s   (* 0.5 tri_base_m_s tri_h_m_s)
                               tri_cnt      (1+ tri_cnt)
                               tri_lbl_s    (strcat *AAF_pfx_tri* (itoa tri_cnt))
                               tri_ctr_s    (ACA-centroid3 pt_a ctr pt_b))
                         (setq sub_shapes (append sub_shapes
                           (list (list tri_lbl_s
                             (strcat tri_lbl_s ": " (rtos tri_base_m_s 2 2)
                                         " × " (rtos tri_h_m_s 2 2)
                                         " / 2 = " (rtos tri_area_s 2 2) "m?")
                             tri_area_s "height" (list ctr tri_foot_s pt_a pt_b) tri_ctr_s 1)))))
                       ;; ---- 弓形（|θ| ? 180°）----
                       (progn
                         (setq bow_cnt  (1+ bow_cnt)
                               lbl      (strcat *AAF_pfx_segment* (itoa bow_cnt))
                               seg_a_m2 (* 0.5 r_m r_m (- (abs theta) (sin (abs theta))))
                               arc_midpt (ACA-arc-midpt pt_a pt_b bv)
                               sctr     (ACA-segment-centroid ctr arc_midpt r theta)
                               ;; 弓形若往內凹，符號為負
                               lbl-sign (if (= arc_sign 1) "" "-"))
                         (setq sub_shapes (append sub_shapes
                           (list (list lbl
                             (strcat lbl ": (θ-sinθ)/2×r?  r=" (rtos r_m 2 2)
                                         "m  θ=" (rtos theta_deg 2 1)
                                         "° (" (rtos theta_deg 2 1)
                                         "-sin" (rtos theta_deg 2 1)
                                         "°)/2×(" (rtos r_m 2 2)
                                         ")?=" (rtos seg_a_m2 2 2) "m?")
                             seg_a_m2 "segment"
                             (list pt_a pt_b ctr r_m theta_deg arc_midpt)
                             sctr arc_sign)))))))
                 )
                 (setq k (1+ k)))

               ;; 步驟2：迭代辨識矩形/梯形子塊，剩餘走耳切三角剖分
               ;; 演算法：反覆掃 poly_pts，找出任何連續 4 點構成「凸的矩形/梯形」、
               ;;        且抽取後產生的對角線 (P0,P3) 不會穿越剩餘多邊形的邊
               ;;        → 抽取為一個子形狀，從 poly_pts 中移除中間兩點 (P1,P2)
               ;;        → 重複，直到找不到可抽取的子塊為止
               ;; 優先級：矩形 > 梯形 (整圈先掃矩形,沒有矩形才接受梯形)
               (setq run_ear_clip t)
               (while
                 (and (>= (length poly_pts) 4)
                      (progn
                        (setq found_quad nil q_n (length poly_pts))
                        ;; 兩階段：accept_pass=1 只接受矩形，accept_pass=2 接受矩形或梯形
                        (setq accept_pass 1)
                        (while (and (<= accept_pass 2) (not found_quad))
                          (setq q_idx 0)
                          (while (and (< q_idx q_n) (not found_quad))
                            (setq qP0 (nth q_idx poly_pts)
                                  qP1 (nth (rem (+ q_idx 1) q_n) poly_pts)
                                  qP2 (nth (rem (+ q_idx 2) q_n) poly_pts)
                                  qP3 (nth (rem (+ q_idx 3) q_n) poly_pts)
                                  quad_type (ACA-classify-quad qP0 qP1 qP2 qP3))
                            ;; 第一輪只接受 rect，第二輪 rect 或 trap 都接受
                            (if (and quad_type
                                     (or (equal quad_type "rect")
                                         (and (= accept_pass 2) (equal quad_type "trap"))))
                              ;; 還要檢查：若 poly_pts > 4，抽掉這塊後的對角線 (qP0,qP3)
                              ;; 不能與多邊形其他邊交叉(否則會破壞拓樸)
                              (if (or (= q_n 4)
                                      (not (ACA-seg-crosses-poly qP0 qP3 poly_pts)))
                                (setq found_quad t)))
                            (if (not found_quad) (setq q_idx (1+ q_idx))))
                          (if (not found_quad) (setq accept_pass (1+ accept_pass))))
                        found_quad))
                 ;; 找到一個可抽取的四邊形，建立子形狀
                 (setq sctr (ACA-poly-centroid (list qP0 qP1 qP2 qP3)))
                 (cond
                   ((equal quad_type "rect")
                    (setq d01 (distance qP0 qP1)
                          d12 (distance qP1 qP2)
                          w_m (/ d01 sc) h_m (/ d12 sc) rect_area (* w_m h_m)
                          rect_cnt (1+ rect_cnt)
                          lbl (strcat *AAF_pfx_rect* (itoa rect_cnt)))
                    (setq sub_shapes (append sub_shapes
                      (list (list lbl
                                  (strcat lbl ": " (rtos w_m 2 2) " × " (rtos h_m 2 2) " = " (rtos rect_area 2 2) "m?")
                                  rect_area "rect" (list qP0 qP1 qP2 qP3) sctr (if has_major_sector -1 1))))))
                   ((equal quad_type "trap")
                    ;; 找出哪一組對邊平行
                    (setq a1 (angle qP0 qP1)
                          a2 (angle qP1 qP2)
                          a3 (angle qP2 qP3)
                          a4 (angle qP3 qP0))
                    (if (< (abs (sin (- a1 a3))) 0.02)
                      (setq b1_pts (list qP0 qP1) b2_pts (list qP2 qP3))
                      (setq b1_pts (list qP1 qP2) b2_pts (list qP3 qP0)))
                    (setq b1_m (/ (distance (car b1_pts) (cadr b1_pts)) sc)
                          b2_m (/ (distance (car b2_pts) (cadr b2_pts)) sc)
                          trap_area (abs (ACA-signed-area (list qP0 qP1 qP2 qP3)))
                          trap_area_m (/ trap_area (* sc sc))
                          h_trap_m (/ (* trap_area_m 2.0) (+ b1_m b2_m))
                          trap_cnt (1+ trap_cnt)
                          lbl (strcat *AAF_pfx_trap* (itoa trap_cnt)))
                    (setq sub_shapes (append sub_shapes
                      (list (list lbl
                                  (strcat lbl ": (" (rtos b1_m 2 2) "+" (rtos b2_m 2 2) ") × " (rtos h_trap_m 2 2) " / 2 = " (rtos trap_area_m 2 2) "m?")
                                  trap_area_m "trap" (list (car b1_pts) (cadr b1_pts) (car b2_pts) (cadr b2_pts)) sctr (if has_major_sector -1 1)))))))
                 ;; 若多邊形原本 > 4 點，抽取後新增一條對角切割線 (qP0,qP3)
                 (if (> q_n 4)
                   (if (not (ACA-is-poly-edge qP0 qP3 poly_pts))
                     (setq diag_lines (append diag_lines (list (list qP0 qP3))))))
                 ;; 從 poly_pts 移除中間兩個點 (P1, P2)，保留 P0 與 P3
                 ;; 須處理環狀索引：若 q_idx+1 或 q_idx+2 超出範圍，要 rem
                 (setq idx_rm1 (rem (+ q_idx 1) q_n)
                       idx_rm2 (rem (+ q_idx 2) q_n))
                 ;; 從大到小移除，避免索引偏移
                 (if (> idx_rm1 idx_rm2)
                   (setq poly_pts (ACA-remove-nth (ACA-remove-nth poly_pts idx_rm1) idx_rm2))
                   (setq poly_pts (ACA-remove-nth (ACA-remove-nth poly_pts idx_rm2) idx_rm1)))
                 ;; 若已抽完(剩 < 3 點)，停止耳切
                 (if (< (length poly_pts) 3) (setq run_ear_clip nil))
               ) ; end while

               (if (and run_ear_clip (>= (length poly_pts) 3))
                 (progn
                   (setq triangles (ACA-ear-clip poly_pts))
                   ;; 2a. 資料收集（不繪製）
                   (foreach tri triangles
                     (setq tA (nth 0 tri) tB (nth 1 tri) tC (nth 2 tri))
                     (setq t_area
                       (abs (/ (- (* (- (car tB) (car tA)) (- (cadr tC) (cadr tA)))
                                  (* (- (car tC) (car tA)) (- (cadr tB) (cadr tA))))
                                2.0)))
                     ;; 過濾退化三角形 (三點共線或近共線造成面積 ? 0)
                     ;; 閾值: 原多邊形總面積的 0.01%，且絕對值 > 1e-6 圖紙單位平方
                     (if (and (> t_area 1e-6)
                              (> t_area (* (abs (vla-get-area obj)) 1e-4)))
                       (progn
                     (setq d01 (distance tA tB)
                           d12 (distance tB tC)
                           d20 (distance tC tA))
                     (cond
                      ;; 直角在 C (斜邊 d01)
                      ((< (abs (- (+ (* d12 d12) (* d20 d20)) (* d01 d01))) (* 1e-3 d12 d20))
                       (setq base_pt1 tB base_pt2 tC apex tA base_len d12))
                      ;; 直角在 A (斜邊 d12)
                      ((< (abs (- (+ (* d01 d01) (* d20 d20)) (* d12 d12))) (* 1e-3 d01 d20))
                       (setq base_pt1 tA base_pt2 tB apex tC base_len d01))
                      ;; 直角在 B (斜邊 d20)
                      ((< (abs (- (+ (* d01 d01) (* d12 d12)) (* d20 d20))) (* 1e-3 d01 d12))
                       (setq base_pt1 tA base_pt2 tB apex tC base_len d01))
                      ;; 預設找最長邊當底
                      ((and (>= d01 d12) (>= d01 d20))
                       (setq base_pt1 tA base_pt2 tB apex tC base_len d01))
                      ((and (>= d12 d01) (>= d12 d20))
                       (setq base_pt1 tB base_pt2 tC apex tA base_len d12))
                       (t
                        (setq base_pt1 tC base_pt2 tA apex tB base_len d20)))
                     (setq foot         (ACA-perp-foot apex base_pt1 base_pt2)
                           base_len_m   (/ base_len sc)
                           height_len_m (/ (distance apex foot) sc)
                           tri_result   (* 0.5 base_len_m height_len_m)
                           tri_cnt      (1+ tri_cnt)
                           lbl          (strcat *AAF_pfx_tri* (itoa tri_cnt))
                           tri_ctr      (ACA-centroid3 tA tB tC))
                     ;; 加入計算清單 (若存在主扇形，符號反轉為 -1)
                     (setq sub_shapes (append sub_shapes
                       (list (list lbl
                         (strcat lbl ": " (rtos base_len_m 2 2)
                                     " × " (rtos height_len_m 2 2)
                                     " / 2 = " (rtos tri_result 2 2) "m?")
                         tri_result "height" (list apex foot base_pt1 base_pt2) tri_ctr (if has_major_sector -1 1)))))
                     ;; 收集對角輔助線（跳過多邊形原始邊）
                     (if (not (ACA-is-poly-edge tA tB poly_pts))
                       (setq diag_lines (append diag_lines (list (list tA tB)))))
                     (if (not (ACA-is-poly-edge tB tC poly_pts))
                       (setq diag_lines (append diag_lines (list (list tB tC)))))
                     (if (not (ACA-is-poly-edge tC tA poly_pts))
                       (setq diag_lines (append diag_lines (list (list tC tA)))))
                       ) ; end progn (面積足夠)
                       (princ "\n[DEBUG] 跳過退化三角形 (面積?0)")
                     ) ; end if 面積過濾
                   ) ; end foreach tri
                 )) ; end progn + if run_ear_clip
              ;; 2b. 統一繪製所有切割線 (不受 run_ear_clip 限制，確保扇形弦線等一定會畫)
              (setvar "clayer" "AREA-切割線")
              (foreach dl diag_lines
                (command "line" "_non" (car dl) "_non" (cadr dl) ""))
              )
             (princ "\n[DEBUG] 形狀分析完成，開始繪製圖面尺寸與標註...")
            ) ; end cond

            ;; C. 繪製輔助線 + 垂直線 + 各項尺寸標註 + 子形狀編號
            ;; aux_pts 結構：
            ;;   "sector"  → (pt_a  ctr   pt_b  r_m  theta_deg  arc_midpt)
            ;;   "segment" → (pt_a  pt_b  ctr   r_m  theta_deg  arc_midpt)
            ;;   "height"  → (apex  foot  bpt1  bpt2)
            (foreach shape sub_shapes
              (setq aux_type (nth 3 shape)
                    aux_pts  (nth 4 shape)
                    sctr     (nth 5 shape))
              (cond

                ;; ---- 扇形 ----
                ((equal aux_type "sector")
                 ;; 繪製圓心標記
                 (setvar "clayer" "AREA-符號")
                 (ACA-draw-center-mark (nth 1 aux_pts) (* txt_h *AAF_sym_scale*))
                 ;; 兩條半徑線 -> 輔助線
                 (setvar "clayer" "AREA-輔助線")
                 (command "line" "_non" (nth 0 aux_pts) "_non" (nth 1 aux_pts) "")
                 (command "line" "_non" (nth 2 aux_pts) "_non" (nth 1 aux_pts) "")
                 ;; 半徑標註：pt_a→ctr 中點，中下對齊，平行半徑線
                 (setvar "clayer" "AREA-數值")
                 (setq midp     (ACA-midpt (nth 0 aux_pts) (nth 1 aux_pts))
                       ang      (angle (nth 0 aux_pts) (nth 1 aux_pts))
                       disp_ang (if (and (> ang (/ pi 2)) (<= ang (* 1.5 pi)))
                                    (+ ang pi) ang)
                       offset_ang (+ disp_ang (/ pi 2)))
                 (ACA-draw-text "bc" (polar midp offset_ang (* txt_h 0.2)) txt_h (* (/ disp_ang pi) 180.0) (strcat "r=" (rtos (nth 3 aux_pts) 2 2)))
                 ;; 角度標註：鎖定於弧頂，依凹凸方向決定對齊
                 (setq ang      (angle (nth 0 aux_pts) (nth 2 aux_pts))
                       disp_ang (if (and (> ang (/ pi 2)) (<= ang (* 1.5 pi))) (+ ang pi) ang))
                 (ACA-draw-text (if (> (nth 6 shape) 0) "bc" "tc") (nth 5 aux_pts) txt_h (* (/ disp_ang pi) 180.0) (strcat (rtos (nth 4 aux_pts) 2 1) "°")))

                ;; ---- 弓形 ----
                ((equal aux_type "segment")
                 ;; 繪製圓心標記
                 (setvar "clayer" "AREA-符號")
                 (ACA-draw-center-mark (nth 2 aux_pts) (* txt_h *AAF_sym_scale*))
                 ;; 弦線 -> 切割線
                 (setvar "clayer" "AREA-切割線")
                 (command "line" "_non" (nth 0 aux_pts) "_non" (nth 1 aux_pts) "")
                 ;; 一條半徑線 pt_a → ctr -> 輔助線
                 (setvar "clayer" "AREA-輔助線")
                 (command "line" "_non" (nth 0 aux_pts) "_non" (nth 2 aux_pts) "")
                 ;; 半徑標註：pt_a→ctr 中點，中下對齊，平行半徑線
                 (setvar "clayer" "AREA-數值")
                 (setq midp     (ACA-midpt (nth 0 aux_pts) (nth 2 aux_pts))
                       ang      (angle (nth 0 aux_pts) (nth 2 aux_pts))
                       disp_ang (if (and (> ang (/ pi 2)) (<= ang (* 1.5 pi)))
                                    (+ ang pi) ang)
                       offset_ang (+ disp_ang (/ pi 2)))
                 (ACA-draw-text "bc" (polar midp offset_ang (* txt_h 0.2)) txt_h (* (/ disp_ang pi) 180.0) (strcat "r=" (rtos (nth 3 aux_pts) 2 2)))
                 ;; 角度標註：鎖定於弧頂，依凹凸方向決定對齊
                 (setq ang      (angle (nth 0 aux_pts) (nth 1 aux_pts))
                       disp_ang (if (and (> ang (/ pi 2)) (<= ang (* 1.5 pi))) (+ ang pi) ang))
                 (ACA-draw-text (if (> (nth 6 shape) 0) "bc" "tc") (nth 5 aux_pts) txt_h (* (/ disp_ang pi) 180.0) (strcat (rtos (nth 4 aux_pts) 2 1) "°")))

                ;; ---- 三角形（垂直線高） ----
                ((equal aux_type "height")
                 ;; 繪製垂直符號
                 (setvar "clayer" "AREA-符號")
                 (ACA-draw-perp-symbol (nth 1 aux_pts) (nth 0 aux_pts) (nth 2 aux_pts) (nth 3 aux_pts) (* txt_h *AAF_sym_scale*))
                 ;; 高線 apex→foot -> 輔助線
                 (setvar "clayer" "AREA-輔助線")
                 (command "line" "_non" (nth 0 aux_pts) "_non" (nth 1 aux_pts) "")
                 (setvar "clayer" "AREA-數值")
                 ;; 底邊長標註：bpt1→bpt2 中點，中下對齊，平行底邊
                 (setq hbm     (/ (distance (nth 2 aux_pts) (nth 3 aux_pts)) sc)
                       midp    (ACA-midpt (nth 2 aux_pts) (nth 3 aux_pts))
                       ang      (angle (nth 2 aux_pts) (nth 3 aux_pts))
                       disp_ang (if (and (> ang (/ pi 2)) (<= ang (* 1.5 pi)))
                                    (+ ang pi) ang)
                       offset_ang (+ disp_ang (/ pi 2)))
                 (ACA-draw-text "bc" (polar midp offset_ang (* txt_h 0.2)) txt_h (* (/ disp_ang pi) 180.0) (rtos hbm 2 2))
                 ;; 垂直線長標註：apex→foot 中點，左下對齊，文字角度垂直於線
                 (setq hhm     (/ (distance (nth 0 aux_pts) (nth 1 aux_pts)) sc)
                       midp    (ACA-midpt (nth 0 aux_pts) (nth 1 aux_pts))
                       ang      (angle (nth 1 aux_pts) (nth 0 aux_pts)) ; 從垂足(foot)到頂點(apex)的角度
                       txt_ang  (- ang (/ pi 2)) ; 文字角度：垂直於線
                       disp_ang (if (and (> txt_ang (/ pi 2)) (<= txt_ang (* 1.5 pi)))
                                    (+ txt_ang pi) txt_ang)
                       offset_ang disp_ang) ; offset_ang 與文字方向相同，確保bl對齊時文字往外擴展
                 (ACA-draw-text "bl" (polar midp offset_ang (* txt_h 0.2)) txt_h (* (/ disp_ang pi) 180.0) (rtos hhm 2 2)))

                ;; ---- 梯形 (僅標註上下底與高) ----
                ((equal aux_type "trap")
                 (setvar "clayer" "AREA-數值")
                 (if skip_edges
                   (progn
                     ;; 底 1 標註
                     (setq midp     (ACA-midpt (nth 0 aux_pts) (nth 1 aux_pts))
                           ang      (angle (nth 0 aux_pts) (nth 1 aux_pts))
                           disp_ang (if (and (> ang (/ pi 2)) (<= ang (* 1.5 pi))) (+ ang pi) ang)
                           offset_ang (+ disp_ang (/ pi 2)))
                     (ACA-draw-text "bc" (polar midp offset_ang (* txt_h 0.2)) txt_h (* (/ disp_ang pi) 180.0) (rtos (/ (distance (nth 0 aux_pts) (nth 1 aux_pts)) sc) 2 2))
                     ;; 底 2 標註
                     (setq midp     (ACA-midpt (nth 2 aux_pts) (nth 3 aux_pts))
                           ang      (angle (nth 2 aux_pts) (nth 3 aux_pts))
                           disp_ang (if (and (> ang (/ pi 2)) (<= ang (* 1.5 pi))) (+ ang pi) ang)
                           offset_ang (+ disp_ang (/ pi 2)))
                     (ACA-draw-text "bc" (polar midp offset_ang (* txt_h 0.2)) txt_h (* (/ disp_ang pi) 180.0) (rtos (/ (distance (nth 2 aux_pts) (nth 3 aux_pts)) sc) 2 2))
                   )
                 )
                 ;; 畫高 (尋找落在底線內側的最佳頂點作為垂足起點)
                 (setq foot1  (ACA-perp-foot (nth 2 aux_pts) (nth 0 aux_pts) (nth 1 aux_pts))
                       foot2  (ACA-perp-foot (nth 3 aux_pts) (nth 0 aux_pts) (nth 1 aux_pts))
                       d_base (distance (nth 0 aux_pts) (nth 1 aux_pts)))
                 (if (< (abs (- (+ (distance (nth 0 aux_pts) foot1) (distance (nth 1 aux_pts) foot1)) d_base)) 1e-4)
                   (setq apex (nth 2 aux_pts) foot foot1)
                   (setq apex (nth 3 aux_pts) foot foot2))
                 (setvar "clayer" "AREA-輔助線") (command "line" "_non" apex "_non" foot "")
                 (setvar "clayer" "AREA-符號") (ACA-draw-perp-symbol foot apex (nth 0 aux_pts) (nth 1 aux_pts) (* txt_h *AAF_sym_scale*))
                 (setvar "clayer" "AREA-數值")
                 (setq midp (ACA-midpt apex foot) ang (angle foot apex) txt_ang (- ang (/ pi 2)) disp_ang (if (and (> txt_ang (/ pi 2)) (<= txt_ang (* 1.5 pi))) (+ txt_ang pi) txt_ang))
                 (ACA-draw-text "bl" (polar midp disp_ang (* txt_h 0.2)) txt_h (* (/ disp_ang pi) 180.0) (rtos (/ (distance apex foot) sc) 2 2)))

                ;; ---- 矩形 (標註「最左頂點的兩條鄰邊」，旋轉一致；自動跳過弓形的弦) ----
                ((equal aux_type "rect")
                 (setvar "clayer" "AREA-數值")
                 (setq rctr_m (ACA-poly-centroid aux_pts))
                 ;; 找 X 最小的頂點 (X 並列取 Y 較小)
                 (setq base_idx 0 k 1)
                 (repeat 3
                   (setq pt_a (nth k aux_pts) pt_b (nth base_idx aux_pts))
                   (if (or (< (car pt_a) (- (car pt_b) 1e-6))
                           (and (< (abs (- (car pt_a) (car pt_b))) 1e-6)
                                (< (cadr pt_a) (cadr pt_b))))
                     (setq base_idx k))
                   (setq k (1+ k)))
                 (setq base_pt (nth base_idx aux_pts)
                       next_pt (nth (rem (1+ base_idx) 4) aux_pts)
                       prev_pt (nth (rem (+ base_idx 3) 4) aux_pts))
                 ;; 一律標最左頂點的兩鄰邊 (AB、AD)，不排除弦
                 ;; 標註邊 1: base → next (AB)
                 (setq midp (ACA-midpt base_pt next_pt)
                       ang (angle base_pt next_pt)
                       disp_ang (if (and (> ang (/ pi 2)) (<= ang (* 1.5 pi))) (+ ang pi) ang)
                       offset_ang (angle rctr_m midp))
                 (ACA-draw-text "bc" (polar midp offset_ang (* txt_h 0.2)) txt_h (* (/ disp_ang pi) 180.0) (rtos (/ (distance base_pt next_pt) sc) 2 2))
                 ;; 標註邊 2: prev → base (AD)
                 (setq midp (ACA-midpt prev_pt base_pt)
                       ang (angle prev_pt base_pt)
                       disp_ang (if (and (> ang (/ pi 2)) (<= ang (* 1.5 pi))) (+ ang pi) ang)
                       offset_ang (angle rctr_m midp))
                 (ACA-draw-text "bc" (polar midp offset_ang (* txt_h 0.2)) txt_h (* (/ disp_ang pi) 180.0) (rtos (/ (distance prev_pt base_pt) sc) 2 2)))

              ) ; end cond aux_type

              ;; 子形狀編號標籤
              (if (and sctr (/= (nth 0 shape) "")) ; 只有當標籤不為空時才繪製
                (progn
                  (setvar "clayer" "AREA-編號")
                  (command "circle" "_non" sctr (* txt_h 0.85)) ; 繪製圓形外框
                  (ACA-draw-text "mc" sctr (* txt_h 0.7) 0.0 (nth 0 shape))))
            )

            ;; 邊長標註 (外框)
            ;; 跳過已屬於「矩形子形狀」或「弓形的弦」的邊，避免重複與不需要的標註
            (if (not skip_edges)
              (progn
                (setvar "clayer" "AREA-數值")
                ;; 收集所有矩形子形狀佔用的 4 條邊 + 所有弓形的弦線
                (setq rect_used_edges nil)
                (foreach shape sub_shapes
                  (cond
                    ;; 矩形：4 條邊都屬於它
                    ((equal (nth 3 shape) "rect")
                     (setq rap (nth 4 shape) rk 0)
                     (repeat 4
                       (setq rect_used_edges (append rect_used_edges
                         (list (list (nth rk rap) (nth (rem (1+ rk) 4) rap)))))
                       (setq rk (1+ rk))))
                    ;; 弓形：aux_pts 的前兩個點 (pt_a, pt_b) 就是弦
                    ((equal (nth 3 shape) "segment")
                     (setq rap (nth 4 shape))
                     (setq rect_used_edges (append rect_used_edges
                       (list (list (nth 0 rap) (nth 1 rap))))))
                  ))
                (setq k 0)
                (repeat n
                  (setq pt_a    (nth k pts_list)
                        pt_b    (if (< (1+ k) n) (nth (1+ k) pts_list) (nth 0 pts_list)))
                  ;; 檢查此邊是否被佔用 (只比對 X、Y 分量，忽略 Z 避免精度問題)
                  (setq edge_skip nil)
                  (foreach re rect_used_edges
                    ;; (a) 端點相同 (容差 1e-4)
                    (if (or (and (< (abs (- (car pt_a) (car (car re)))) 1e-4)
                                 (< (abs (- (cadr pt_a) (cadr (car re)))) 1e-4)
                                 (< (abs (- (car pt_b) (car (cadr re)))) 1e-4)
                                 (< (abs (- (cadr pt_b) (cadr (cadr re)))) 1e-4))
                            (and (< (abs (- (car pt_a) (car (cadr re)))) 1e-4)
                                 (< (abs (- (cadr pt_a) (cadr (cadr re)))) 1e-4)
                                 (< (abs (- (car pt_b) (car (car re)))) 1e-4)
                                 (< (abs (- (cadr pt_b) (cadr (car re)))) 1e-4)))
                      (setq edge_skip t)
                      ;; (b) 與該佔用邊共線 (pt_a、pt_b 到該邊所在直線的垂距都近 0)
                      (progn
                        (setq ref_a (car re) ref_b (cadr re))
                        (setq ref_len (distance ref_a ref_b))
                        (if (> ref_len 1e-6)
                          (progn
                            (setq foot_a (ACA-perp-foot pt_a ref_a ref_b)
                                  foot_b (ACA-perp-foot pt_b ref_a ref_b))
                            (if (and (< (distance pt_a foot_a) (* ref_len 1e-3))
                                     (< (distance pt_b foot_b) (* ref_len 1e-3)))
                              (setq edge_skip t)))))))
                  ;; 進一步檢查：此邊是否與某條弓形弦「共線」(是弦被分割後的殘段)
                  ;; 若是，該長度對計算式無意義，跳過標註
                  (if (not edge_skip)
                    (foreach shape sub_shapes
                      (if (equal (nth 3 shape) "segment")
                        (progn
                          (setq chord_a (nth 0 (nth 4 shape))
                                chord_b (nth 1 (nth 4 shape)))
                          ;; 共線判斷：pt_a 與 pt_b 兩點到 (chord_a, chord_b) 直線的垂距都接近 0
                          (setq foot_a (ACA-perp-foot pt_a chord_a chord_b)
                                foot_b (ACA-perp-foot pt_b chord_a chord_b)
                                chord_len (distance chord_a chord_b))
                          (if (and (> chord_len 1e-6)
                                   (< (distance pt_a foot_a) (* chord_len 1e-3))
                                   (< (distance pt_b foot_b) (* chord_len 1e-3)))
                            (setq edge_skip t))))))
                  (if (not edge_skip)
                    (progn
                      (setq dist    (/ (distance pt_a pt_b) sc)
                            midp    (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pt_a pt_b)
                            ang     (angle pt_a pt_b)
                            disp_ang (if (and (> ang (/ pi 2)) (<= ang (* 1.5 pi)))
                                         (+ ang pi) ang)
                            offset_ang (+ disp_ang (/ pi 2)))
                      (ACA-draw-text "bc" (polar midp offset_ang (* txt_h 0.2)) txt_h (* (/ disp_ang pi) 180.0) (rtos dist 2 2))))
                  (setq k (1+ k)))
              )
            )

            ;; 主圓圈編號
            (setvar "clayer" "AREA-編號")
            ;; 若只有一個子形狀(如單一弓形/扇形)，以該子形狀中心為主編號基準，
            ;; 避免 poly-centroid 在非凸圖形上計算得到的重心落在圖外或不恰當位置
            (if (and (= (length sub_shapes) 1) (nth 5 (car sub_shapes)))
              (setq cp (nth 5 (car sub_shapes)))
              (setq cp (ACA-poly-centroid pts_list)))
            ;; 偵測是否有子編號重心與主編號重心距離過近 (< 主編號圓 + 子編號圓 半徑和)
            ;; 若有，將主編號向上偏移子編號圓直徑的距離 (txt_h * 2.4)
            (setq min_dist (* txt_h 2.1)  ; 主圓 1.2 + 子圓 0.85 + 微距 = 約 2.1
                  need_offset nil)
            (foreach shape sub_shapes
              (if (and (nth 5 shape) (/= (nth 0 shape) ""))
                (if (< (distance cp (nth 5 shape)) min_dist)
                  (setq need_offset t))))
            (if need_offset
              (setq cp (list (car cp) (+ (cadr cp) (* txt_h 2.4)) 0.0)))
            (command "circle" "_non" cp (* txt_h 1.2))
            (ACA-draw-text "mc" cp txt_h 0.0 (itoa (1+ i))) ; 主編號文字

            (princ "\n[DEBUG] 圖面標註完成，準備將計算式輸出至表格...")
            ;; D. 輸出計算表（公式列）
            (setvar "clayer" "AREA-文字")
            (ACA-draw-text "l" row_pt txt_h 0.0 (itoa (1+ i))) ; 寫編號
            (ACA-draw-text "l" (polar row_pt 0 (+ col_w1 col_w2)) txt_h 0.0 (rtos area 2 3)) ; 寫面積
            ;; 第一個 shape 的計算式寫在編號同一列,後續才下移
            (setq first_entry t)
            (foreach shape sub_shapes
              (if first_entry
                (setq first_entry nil)
                (setq row_pt (polar row_pt (/ pi -2) (* txt_h *AAF_sub_space*))))
              (ACA-draw-text "l" (polar row_pt 0 col_w1) txt_h 0.0 (nth 1 shape)))

            ;; E. 總和計算式（只在多個子形狀時輸出）
            (if (> (length sub_shapes) 1)
              (progn
                (setq sum_total 0.0 i_shape 0 num_shapes (length sub_shapes)
                      lbl_lines nil val_lines nil cur_lbl "" cur_val "")
                (while (< i_shape num_shapes)
                  (setq shape (nth i_shape sub_shapes)
                        sgn   (nth 6 shape)
                        shp_a (nth 2 shape)
                        lbl   (nth 0 shape))
                  (setq sum_total (+ sum_total (* sgn shp_a)))
                  
                  (if (= (rem i_shape 10) 0)
                    (if (= i_shape 0)
                      (setq cur_lbl (if (> sgn 0) lbl (strcat "- " lbl))
                            cur_val (if (> sgn 0) (rtos shp_a 2 2) (strcat "- " (rtos shp_a 2 2))))
                      (setq cur_lbl (strcat (if (> sgn 0) "+ " "- ") lbl)
                            cur_val (strcat (if (> sgn 0) "+ " "- ") (rtos shp_a 2 2)))
                    )
                    (setq cur_lbl (strcat cur_lbl (if (> sgn 0) " + " " - ") lbl)
                          cur_val (strcat cur_val (if (> sgn 0) " + " " - ") (rtos shp_a 2 2)))
                  )
                  (setq i_shape (1+ i_shape))
                  (if (or (= (rem i_shape 10) 0) (= i_shape num_shapes))
                    (setq lbl_lines (append lbl_lines (list cur_lbl))
                          val_lines (append val_lines (list cur_val)))
                  )
                )

                ;; 輸出分段後的標籤與數值行
                (foreach line lbl_lines
                  (setq row_pt (polar row_pt (/ pi -2) (* txt_h *AAF_sub_space*)))
                  (ACA-draw-text "l" (polar row_pt 0 col_w1) txt_h 0.0 line))
                (setq first_val t)
                (foreach line val_lines
                  (setq row_pt (polar row_pt (/ pi -2) (* txt_h *AAF_sub_space*)))
                  (if first_val
                    (progn (ACA-draw-text "l" (polar row_pt 0 col_w1) txt_h 0.0 (strcat "= " line)) (setq first_val nil))
                    (ACA-draw-text "l" (polar row_pt 0 col_w1) txt_h 0.0 line)))
                (setq row_pt (polar row_pt (/ pi -2) (* txt_h *AAF_sub_space*)))
                (ACA-draw-text "l" (polar row_pt 0 col_w1) txt_h 0.0 (strcat "= " (rtos sum_total 2 2) "m?"))
              )
            )

            ;; 統一調整與下一個多邊形計算式的段落間距
            (setq row_pt (polar row_pt (/ pi -2) (* txt_h *AAF_main_space*)))
            (setq i (1+ i))
            (princ (strcat "\n[DEBUG] 第 " (itoa i) " 個圖形處理成功！"))
              ) ; end progn (正常處理)
             ) ; end t 分支
            ) ; end cond (防蠢機制)
          ) ; end repeat

          ;; 總計欄
          (setvar "clayer" "AREA-文字")
          (command "line" "_non"
            (polar row_pt 0 -2) "_non"
            (polar row_pt 0 (+ col_w1 col_w2 15)) "")
          (setq row_pt (polar row_pt (/ pi -2) (* txt_h *AAF_main_space*)))
          (ACA-draw-text "l" row_pt txt_h 0.0 "Total Area:")
          (ACA-draw-text "l" (polar row_pt 0 (+ col_w1 col_w2)) txt_h 0.0
            (rtos total_area 2 3))
        )
      )
    )
  )
  
  ;; 恢復系統變數
  (if old_osmode (setvar "osmode" old_osmode))
  (setq *error* old_err)
  
  (command "_undo" "_end")
  (setvar "cmdecho" 1)
  (princ "\n[ACA 完成] 計算式已標註於圖面。")
  (princ))

;; 重置全域設定
(defun c:AutoCalAreaReset ()
  (setq *AAF_txt_h* nil *AAF_sc* nil *AAF_main_space* nil *AAF_sub_space* nil *AAF_sym_scale* nil
        *AAF_pfx_sector* nil *AAF_pfx_segment* nil *AAF_pfx_rect* nil *AAF_pfx_tri* nil *AAF_pfx_trap* nil)
  (princ "\n[ACA] 所有設定已重置為預設值。")
  (princ))

;; --- 設定選項指令 ---
(defun c:AutoCalAreaOption (/ kw loop val)
  (if (not *AAF_txt_h*) (setq *AAF_txt_h* 15.0))
  (if (not *AAF_sc*)    (setq *AAF_sc* 100.0))
  (if (not *AAF_main_space*) (setq *AAF_main_space* 2.0))
  (if (not *AAF_sub_space*)  (setq *AAF_sub_space* 1.8))
  (if (not *AAF_sym_scale*)  (setq *AAF_sym_scale* 0.8))
  
  (setq loop t)
  (while loop
    (initget "TextHeight UnitScale MainrowSpacing SubrowSpacing sYmbolScale eXit")
    (setq kw (getkword "\n選擇設定選項 [文字高度(T)/單位比例(U)/主編號行距(M)/子編號行距(S)/符號比例(Y)/離開(X)] <離開>: "))
    (cond
      ((= kw "TextHeight")
       (setq val (getdist (strcat "\n設定文字高度 <" (rtos *AAF_txt_h* 2 2) ">: ")))
       (if val (setq *AAF_txt_h* val)))
      ((= kw "UnitScale")
       (setq val (getreal (strcat "\n設定單位比例 <" (rtos *AAF_sc* 2 2) ">: ")))
       (if val (setq *AAF_sc* val)))
      ((= kw "MainrowSpacing")
       (setq val (getreal (strcat "\n設定主編號行距倍數 <" (rtos *AAF_main_space* 2 2) ">: ")))
       (if val (setq *AAF_main_space* val)))
      ((= kw "SubrowSpacing")
       (setq val (getreal (strcat "\n設定子編號行距倍數 <" (rtos *AAF_sub_space* 2 2) ">: ")))
       (if val (setq *AAF_sub_space* val)))
      ((= kw "sYmbolScale")
       (setq val (getreal (strcat "\n設定符號比例 (相對於文字高度) <" (rtos *AAF_sym_scale* 2 2) ">: ")))
       (if val (setq *AAF_sym_scale* val)))
      (t (setq loop nil)) ; eXit or Enter
    )
  )
  (princ (strcat "\n[ACA 設定狀態] 文字高度:" (rtos *AAF_txt_h* 2 2) 
                 " | 單位比例:" (rtos *AAF_sc* 2 2) 
                 " | 主編號行距:" (rtos *AAF_main_space* 2 2) 
                 " | 子編號行距:" (rtos *AAF_sub_space* 2 2)
                 " | 符號比例:" (rtos *AAF_sym_scale* 2 2)))
  (princ)
)

;; --- 子編號前綴設定指令 ---
;; ACAL = AutoCalArea Label
(defun c:AutoCalAreaLabel (/ kw loop val)
  (if (not *AAF_pfx_sector*)  (setq *AAF_pfx_sector* "S"))
  (if (not *AAF_pfx_segment*) (setq *AAF_pfx_segment* "G"))
  (if (not *AAF_pfx_rect*)    (setq *AAF_pfx_rect* "J"))
  (if (not *AAF_pfx_tri*)     (setq *AAF_pfx_tri* "A"))
  (if (not *AAF_pfx_trap*)    (setq *AAF_pfx_trap* "T"))

  (princ (strcat "\n[ACA 子編號前綴] 扇形:" *AAF_pfx_sector*
                 " | 弓形:" *AAF_pfx_segment*
                 " | 矩形:" *AAF_pfx_rect*
                 " | 三角形:" *AAF_pfx_tri*
                 " | 梯形:" *AAF_pfx_trap*))
  (setq loop t)
  (while loop
    (initget "Sector seGment Rectangle triAngle Trapezoid eXit")
    (setq kw (getkword "\n修改前綴 [扇形(S)/弓形(G)/矩形(R)/三角形(A)/梯形(T)/離開(X)] <離開>: "))
    (cond
      ((= kw "Sector")
       (setq val (getstring (strcat "\n扇形前綴 <" *AAF_pfx_sector* ">: ")))
       (if (/= val "") (setq *AAF_pfx_sector* val)))
      ((= kw "seGment")
       (setq val (getstring (strcat "\n弓形前綴 <" *AAF_pfx_segment* ">: ")))
       (if (/= val "") (setq *AAF_pfx_segment* val)))
      ((= kw "Rectangle")
       (setq val (getstring (strcat "\n矩形前綴 <" *AAF_pfx_rect* ">: ")))
       (if (/= val "") (setq *AAF_pfx_rect* val)))
      ((= kw "triAngle")
       (setq val (getstring (strcat "\n三角形前綴 <" *AAF_pfx_tri* ">: ")))
       (if (/= val "") (setq *AAF_pfx_tri* val)))
      ((= kw "Trapezoid")
       (setq val (getstring (strcat "\n梯形前綴 <" *AAF_pfx_trap* ">: ")))
       (if (/= val "") (setq *AAF_pfx_trap* val)))
      (t (setq loop nil))
    )
  )
  (princ (strcat "\n[ACA 子編號前綴] 扇形:" *AAF_pfx_sector*
                 " | 弓形:" *AAF_pfx_segment*
                 " | 矩形:" *AAF_pfx_rect*
                 " | 三角形:" *AAF_pfx_tri*
                 " | 梯形:" *AAF_pfx_trap*))
  (princ)
)

;; 縮寫別名
(defun c:ACA  () (c:AutoCalArea))
(defun c:ACAR () (c:AutoCalAreaReset))
(defun c:ACAO () (c:AutoCalAreaOption))
(defun c:ACAL () (c:AutoCalAreaLabel))
