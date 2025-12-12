//

//lisp nodetree
以下是一个完整的 Lisp 代码语法树解析和显示工具，可以读取缓冲区或选中区域的 Lisp 代码，将其解析为语法树并以树状结构打印：

;;; ==========================================
;;; Lisp 代码语法树分析工具
;;; ==========================================

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;; 1. 语法树节点结构
(cl-defstruct lisp-tree-node
  "Lisp语法树节点"
  (type nil)      ; 节点类型：symbol, list, number, string, keyword, etc.
  (value nil)     ; 节点的值
  (children nil)  ; 子节点列表
  (position nil)  ; 在源码中的位置 (beg . end)
  (depth 0)       ; 深度
  (parent nil))   ; 父节点

;;; 2. 颜色定义
(defvar tree-colors
  '((:symbol     . "#ffd700")    ; 黄金
    (:keyword    . "#bd93f9")    ; 紫色
    (:string     . "#f1fa8c")    ; 浅金
    (:number     . "#8be9fd")    ; 青色
    (:list       . "#ff5555")    ; 红色
    (:function   . "#50fa7b")    ; 绿色
    (:variable   . "#ffb86c")    ; 橙色
    (:comment    . "#6272a4")    ; 灰色
    (:error      . "#ff3333")    ; 红色
    (:default    . "#f8f8f2"))   ; 奶白
  "语法树节点颜色")

(defun tree-color (name)
  "获取颜色值"
  (cdr (assoc name tree-colors)))

;;; 3. Lisp 代码解析
(defun parse-lisp-code (code)
  "解析Lisp代码字符串，返回语法树"
  (condition-case err
      (with-temp-buffer
        (insert code)
        (goto-char (point-min))
        (let ((nodes nil)
              (position-stack nil))
          
          ;; 读取所有表达式
          (while (not (eobp))
            (skip-chars-forward " \t\n\r")
            (unless (eobp)
              (let ((start (point))
                    (expr (read (current-buffer)))
                    (end (point)))
                (when expr
                  (push (parse-expression expr start end) nodes)))))
          
          (reverse nodes)))
    (error
     (message "解析Lisp代码失败: %s" (error-message-string err))
     nil)))

(defun parse-expression (expr &optional start end)
  "递归解析表达式"
  (cond
   ;; 列表
   ((consp expr)
    (let ((node (make-lisp-tree-node
                 :type 'list
                 :value (if (functionp (car expr)) 'function-call 'list)
                 :position (cons start end)
                 :children nil)))
      (setf (lisp-tree-node-children node)
            (cl-loop for elem in expr
                     for i from 0
                     for child = (parse-expression elem)
                     when child
                     do (setf (lisp-tree-node-parent child) node
                              (lisp-tree-node-depth child) (1+ (lisp-tree-node-depth node)))
                     and collect child))
      node))
   
   ;; 符号
   ((symbolp expr)
    (make-lisp-tree-node
     :type 'symbol
     :value expr
     :position (cons start end)
     :children nil))
   
   ;; 字符串
   ((stringp expr)
    (make-lisp-tree-node
     :type 'string
     :value expr
     :position (cons start end)
     :children nil))
   
   ;; 数字
   ((numberp expr)
    (make-lisp-tree-node
     :type 'number
     :value expr
     :position (cons start end)
     :children nil))
   
   ;; 关键字
   ((keywordp expr)
    (make-lisp-tree-node
     :type 'keyword
     :value expr
     :position (cons start end)
     :children nil))
   
   ;; nil
   ((null expr)
    (make-lisp-tree-node
     :type 'symbol
     :value 'nil
     :position (cons start end)
     :children nil))
   
   ;; 其他
   (t
    (make-lisp-tree-node
     :type 'unknown
     :value expr
     :position (cons start end)
     :children nil))))

;;; 4. 树状显示
(defun print-tree-node (node &optional indent)
  "递归打印语法树节点"
  (let* ((depth (or (lisp-tree-node-depth node) 0))
         (indent-str (make-string (* depth 2) ?\s))
         (type (lisp-tree-node-type node))
         (value (lisp-tree-node-value node))
         (children (lisp-tree-node-children node))
         (formatted-value (format-tree-node-value value type)))
    
    (insert indent-str)
    
    ;; 根据类型着色
    (let ((color (cond
                  ((eq type 'symbol) (tree-color :symbol))
                  ((eq type 'keyword) (tree-color :keyword))
                  ((eq type 'string) (tree-color :string))
                  ((eq type 'number) (tree-color :number))
                  ((eq type 'list) (tree-color :list))
                  (t (tree-color :default)))))
      
      (insert (propertize formatted-value
                          'face `(:foreground ,color
                                  :weight ,(if children 'bold 'normal))
                          'lisp-tree-node node
                          'lisp-tree-depth depth)))
    
    ;; 如果有位置信息，添加位置标记
    (when (lisp-tree-node-position node)
      (let ((pos (lisp-tree-node-position node)))
        (insert (propertize (format " [%d:%d]" (car pos) (cdr pos))
                            'face '(:foreground "#6272a4" :italic t)))))
    
    (insert "\n")
    
    ;; 递归打印子节点
    (dolist (child children)
      (print-tree-node child (1+ (or indent 0))))))

(defun format-tree-node-value (value type)
  "格式化树节点值"
  (cond
   ((eq type 'string)
    (format "%S" value))  ; 字符串加引号
   ((eq type 'list)
    (if (eq value 'function-call)
        "function-call"
      "list"))
   ((null value)
    "nil")
   ((symbolp value)
    (symbol-name value))
   ((keywordp value)
    (concat ":" (symbol-name value)))
   (t
    (format "%s" value))))

;;; 5. 主显示函数
(defun lisp-tree-display (code &optional buffer-name)
  "显示Lisp代码的语法树"
  (let* ((nodes (parse-lisp-code code))
         (buffer (get-buffer-create (or buffer-name "*Lisp Syntax Tree*"))))
    
    (with-current-buffer buffer
      (erase-buffer)
      (lisp-tree-mode)
      
      ;; 添加标题
      (insert (propertize "Lisp 代码语法树分析\n" 
                          'face '(:height 1.5 :weight bold :foreground "#ffd700")))
      (insert "\n")
      
      ;; 如果没有节点，显示错误
      (if (null nodes)
          (progn
            (insert (propertize "错误: 无法解析Lisp代码\n" 
                                'face '(:foreground "#ff5555" :weight bold)))
            (insert "请检查代码语法是否正确。\n"))
        
        ;; 显示统计信息
        (let* ((total-nodes (count-tree-nodes nodes))
               (max-depth (tree-max-depth nodes)))
          (insert (format "表达式数量: %d\n" (length nodes)))
          (insert (format "总节点数: %d\n" total-nodes))
          (insert (format "最大深度: %d\n" max-depth))
          (insert "\n"))
        
        ;; 打印所有树的根节点
        (dolist (node nodes)
          (print-tree-node node))
        
        ;; 添加分隔线
        (insert "\n" (make-string 80 ?-) "\n\n")
        
        ;; 添加符号表
        (insert (propertize "符号表:\n" 'face '(:weight bold :foreground "#ffd700")))
        (let ((symbols (collect-symbols nodes)))
          (if symbols
              (progn
                (cl-loop for (symbol . count) in symbols
                         do (insert (format "  %s: %d 次\n" symbol count))))
            (insert "  (无符号)\n")))
        
        ;; 添加帮助信息
        (insert "\n" (propertize "快捷键:\n" 'face '(:weight bold :foreground "#ffd700")))
        (insert "  n/p - 下一个/上一个节点\n")
        (insert "  t   - 跳转到源码位置\n")
        (insert "  c   - 复制节点\n")
        (insert "  q   - 退出\n"))
      
      (goto-char (point-min)))
    
    (display-buffer buffer)
    buffer))

;;; 6. 工具函数
(defun count-tree-nodes (nodes)
  "统计树中节点总数"
  (cl-labels ((count-node (node)
                (1+ (cl-reduce '+ (lisp-tree-node-children node)
                               :key #'count-node))))
    (cl-reduce '+ nodes :key #'count-node)))

(defun tree-max-depth (nodes)
  "计算树的最大深度"
  (cl-labels ((node-depth (node)
                (if (lisp-tree-node-children node)
                    (1+ (cl-reduce 'max (lisp-tree-node-children node)
                                   :key #'node-depth))
                  0)))
    (cl-reduce 'max nodes :key #'node-depth)))

(defun collect-symbols (nodes)
  "收集所有符号及其出现次数"
  (let ((symbol-table (make-hash-table :test 'equal)))
    (cl-labels ((collect-from-node (node)
                  (when (eq (lisp-tree-node-type node) 'symbol)
                    (let* ((sym (lisp-tree-node-value node))
                           (sym-str (if (keywordp sym)
                                        (concat ":" (symbol-name sym))
                                      (symbol-name sym))))
                      (puthash sym-str (1+ (gethash sym-str symbol-table 0))
                               symbol-table)))
                  (dolist (child (lisp-tree-node-children node))
                    (collect-from-node child))))
      (dolist (node nodes)
        (collect-from-node node)))
    
    ;; 排序并返回列表
    (let (result)
      (maphash (lambda (sym count) (push (cons sym count) result)) symbol-table)
      (sort result (lambda (a b) (> (cdr a) (cdr b)))))))

;;; 7. 主命令函数
(defun lisp-tree-from-buffer ()
  "分析当前缓冲区的Lisp代码"
  (interactive)
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (lisp-tree-display code "*Lisp Syntax Tree*")
    (message "已分析当前缓冲区的Lisp代码")))

(defun lisp-tree-from-region ()
  "分析选中区域的Lisp代码"
  (interactive)
  (if (use-region-p)
      (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
        (lisp-tree-display code "*Lisp Syntax Tree*")
        (message "已分析选中区域的Lisp代码"))
    (message "请先选中一个区域")))

(defun lisp-tree-from-file (filename)
  "从文件分析Lisp代码"
  (interactive "f选择Lisp文件: ")
  (with-temp-buffer
    (insert-file-contents filename)
    (lisp-tree-display (buffer-string)
                       (format "*Lisp Syntax Tree: %s*" (file-name-nondirectory filename)))
    (message "已分析文件: %s" filename)))

;;; 8. 树模式
(defvar lisp-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'lisp-tree-next-node)
    (define-key map (kbd "p") 'lisp-tree-prev-node)
    (define-key map (kbd "t") 'lisp-tree-goto-source)
    (define-key map (kbd "c") 'lisp-tree-copy-node)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "g") 'lisp-tree-refresh)
    map)
  "Lisp树模式键映射")

(define-derived-mode lisp-tree-mode special-mode "Lisp Tree"
  "Lisp语法树显示模式"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (font-lock-mode 1)
  (hl-line-mode 1))

;;; 9. 导航函数
(defun lisp-tree-get-current-node ()
  "获取当前光标位置的节点"
  (get-text-property (point) 'lisp-tree-node))

(defun lisp-tree-next-node ()
  "移动到下一个节点"
  (interactive)
  (let ((pos (next-single-property-change (point) 'lisp-tree-node)))
    (if pos
        (goto-char pos)
      (message "已经是最后一个节点"))))

(defun lisp-tree-prev-node ()
  "移动到上一个节点"
  (interactive)
  (let ((pos (previous-single-property-change (point) 'lisp-tree-node)))
    (if pos
        (goto-char (previous-single-property-change pos 'lisp-tree-node))
      (message "已经是第一个节点"))))

(defun lisp-tree-goto-source ()
  "跳转到源码位置"
  (interactive)
  (let ((node (lisp-tree-get-current-node)))
    (if node
        (let* ((pos (lisp-tree-node-position node))
               (beg (car pos))
               (end (cdr pos)))
          (if (and beg end)
              (progn
                (switch-to-buffer-other-window (marker-buffer (car (buffer-list))))
                (goto-char beg)
                (message "跳转到位置 %d-%d" beg end))
            (message "此节点无位置信息")))
      (message "当前位置无节点"))))

(defun lisp-tree-copy-node ()
  "复制当前节点"
  (interactive)
  (let ((node (lisp-tree-get-current-node)))
    (if node
        (let ((value (lisp-tree-node-value node))
              (type (lisp-tree-node-type node)))
          (kill-new (format "%S" value))
          (message "已复制 %s 节点: %S" type value))
      (message "当前位置无节点"))))

(defun lisp-tree-refresh ()
  "刷新树显示"
  (interactive)
  (let ((buffer (current-buffer)))
    (when (string-match "\\*Lisp Syntax Tree" (buffer-name buffer))
      (bury-buffer buffer)
      (lisp-tree-from-buffer))))

;;; 10. 高级分析功能
(defun lisp-tree-analyze-function-calls (nodes)
  "分析函数调用"
  (let ((func-calls nil))
    (cl-labels ((analyze-node (node)
                  (when (and (eq (lisp-tree-node-type node) 'list)
                             (eq (lisp-tree-node-value node) 'function-call))
                    (let ((children (lisp-tree-node-children node)))
                      (when (and children (eq (lisp-tree-node-type (car children)) 'symbol))
                        (let ((func-name (lisp-tree-node-value (car children)))
                              (arg-count (1- (length children))))
                          (push (list func-name arg-count) func-calls)))))
                  (dolist (child (lisp-tree-node-children node))
                    (analyze-node child))))
      (dolist (node nodes)
        (analyze-node node)))
    func-calls))

(defun lisp-tree-show-function-analysis ()
  "显示函数调用分析"
  (interactive)
  (let* ((code (buffer-substring-no-properties (point-min) (point-max)))
         (nodes (parse-lisp-code code))
         (func-calls (lisp-tree-analyze-function-calls nodes))
         (buffer (get-buffer-create "*Lisp 函数分析*")))
    
    (with-current-buffer buffer
      (erase-buffer)
      (insert (propertize "Lisp 函数调用分析\n" 
                          'face '(:height 1.5 :weight bold :foreground "#ffd700")))
      (insert "\n")
      
      (if func-calls
          (progn
            (insert "| 函数名 | 调用次数 | 平均参数 |\n")
            (insert "|--------+----------+----------|\n")
            
            (let ((func-table (make-hash-table :test 'equal)))
              ;; 统计每个函数的调用情况
              (dolist (call func-calls)
                (let* ((func-name (car call))
                       (arg-count (cadr call))
                       (entry (gethash func-name func-table)))
                  (if entry
                      (setf (car entry) (1+ (car entry))
                            (cdr entry) (+ (cdr entry) arg-count))
                    (puthash func-name (cons 1 arg-count) func-table))))
              
              ;; 输出结果
              (maphash (lambda (func-name entry)
                         (let ((count (car entry))
                               (total-args (cdr entry))
                               (avg-args (float (/ (cdr entry) (car entry)))))
                           (insert (format "| %s | %d | %.1f |\n" 
                                          func-name count avg-args))))
                       func-table)))
        (insert "未发现函数调用\n"))
      
      (org-mode)
      (org-table-align)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    
    (display-buffer buffer)))

;;; 11. 可视化增强
(defun lisp-tree-display-graph (nodes)
  "以图形方式显示语法树"
  (let ((buffer (get-buffer-create "*Lisp 语法树图*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "#+TITLE: Lisp 语法树可视化\n")
      (insert "\n")
      
      (cl-labels ((draw-node (node x y)
                    (let* ((type (lisp-tree-node-type node))
                           (value (lisp-tree-node-value node))
                           (children (lisp-tree-node-children node))
                           (label (format-tree-node-value value type))
                           (color (cond
                                   ((eq type 'symbol) (tree-color :symbol))
                                   ((eq type 'keyword) (tree-color :keyword))
                                   ((eq type 'list) (tree-color :list))
                                   (t (tree-color :default)))))
                      
                      ;; 绘制当前节点
                      (insert (format "[[%d,%d][%s]]\n" x y label))
                      
                      ;; 绘制子节点
                      (when children
                        (let ((child-count (length children))
                              (start-x (- x (floor (/ child-count 2)))))
                          (dotimes (i child-count)
                            (let ((child-x (+ start-x i))
                                  (child-y (1+ y)))
                              ;; 绘制连线
                              (insert (format "[%d,%d] -> [%d,%d]\n" x y child-x child-y))
                              (draw-node (nth i children) child-x child-y))))))))
        
        ;; 绘制所有根节点
        (dotimes (i (length nodes))
          (draw-node (nth i nodes) (* i 3) 0)))
      
      (graphviz-dot-mode)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    
    (display-buffer buffer)))

;;; 12. 全局快捷键
(global-set-key (kbd "C-c l t") 'lisp-tree-from-buffer)
(global-set-key (kbd "C-c l r") 'lisp-tree-from-region)
(global-set-key (kbd "C-c l f") 'lisp-tree-from-file)
(global-set-key (kbd "C-c l a") 'lisp-tree-show-function-analysis)

;;; 13. 示例代码
(defconst lisp-tree-example-code
  "(defun factorial (n)
  \"计算阶乘\"
  (if (<= n 1)
      1
    (* n (factorial (1- n)))))

(defun fibonacci (n)
  \"计算斐波那契数列\"
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (t (+ (fibonacci (- n 1))
         (fibonacci (- n 2))))))

;; 测试函数
(let ((result (factorial 5)))
  (message \"5! = %d\" result))")

(defun lisp-tree-show-example ()
  "显示示例Lisp代码的语法树"
  (interactive)
  (lisp-tree-display lisp-tree-example-code "*Lisp 语法树示例*")
  (message "已显示示例代码的语法树"))

;;; 14. 提供模式
(provide 'lisp-tree)

;;; 15. 初始化
(defun lisp-tree-init ()
  "初始化Lisp语法树系统"
  (message "Lisp语法树系统已加载"))

(run-with-idle-timer 1 nil 'lisp-tree-init)


使用说明

1. 基本使用

分析当前缓冲区 (C-c l t)

M-x lisp-tree-from-buffer


分析整个缓冲区的Lisp代码并显示语法树。

分析选中区域 (C-c l r)

M-x lisp-tree-from-region


先选中一个区域，然后分析选中区域的Lisp代码。

分析文件 (C-c l f)

M-x lisp-tree-from-file


选择Lisp文件并分析。

2. 语法树显示界面

显示格式：

Lisp 代码语法树分析

表达式数量: 3
总节点数: 28
最大深度: 5

defun [1:5]
  factorial [6:15]
  (n) [16:19]
  "计算阶乘" [20:29]
  if [30:32]
    <= [33:35]
      n [36:37]
      1 [38:39]
    1 [40:41]
    * [42:43]
      n [44:45]
      factorial [46:55]
        1- [56:58]
          n [59:60]


颜色编码：

• 符号: 金色 (#ffd700)

• 关键字: 紫色 (#bd93f9)

• 字符串: 浅金色 (#f1fa8c)

• 数字: 青色 (#8be9fd)

• 列表: 红色 (#ff5555)

• 函数调用: 绿色 (#50fa7b)

3. 导航快捷键

在语法树缓冲区中：

• n - 移动到下一个节点

• p - 移动到上一个节点

• t - 跳转到源码位置

• c - 复制当前节点

• q - 退出缓冲区

• g - 刷新显示

4. 高级功能

函数调用分析 (C-c l a)

M-x lisp-tree-show-function-analysis


显示函数调用统计，包括：

• 函数名

• 调用次数

• 平均参数数量

示例代码

M-x lisp-tree-show-example


显示预定义的Lisp示例代码的语法树。

5. 显示的特性

1. 节点信息

每个节点显示：

• 节点值

• 节点类型

• 源码位置 [开始:结束]

2. 缩进层次

使用缩进表示树的深度，每层缩进2个空格。

3. 符号统计

在底部显示所有符号及其出现次数，按出现次数排序。

4. 错误处理

如果代码有语法错误，会显示错误信息而不是崩溃。

6. 示例代码解析

输入代码：

(defun factorial (n)
  (if (<= n 1)
      1
    (* n (factorial (1- n)))))

(factorial 5)


输出语法树：

defun
  factorial
  (n)
  if
    <=
      n
      1
    1
    *
      n
      factorial
        1-
          n
factorial
  5


7. 自定义配置

修改颜色方案

(setq tree-colors
      '((:symbol     . "#ff9900")
        (:keyword    . "#9966ff")
        (:string     . "#99ff99")
        (:number     . "#66ccff")
        (:list       . "#ff6666")
        (:function   . "#66ff99")
        (:variable   . "#ffcc66")))


修改缩进宽度

;; 在 print-tree-node 函数中修改
(make-string (* depth 4) ?\s)  ; 改为4空格缩进


添加节点类型

;; 在 parse-expression 函数中添加
((vectorp expr)
 (make-lisp-tree-node
  :type 'vector
  :value expr
  :position (cons start end)
  :children (mapcar #'parse-expression (append expr nil))))


8. 故障排除

代码解析失败

;; 检查代码语法
M-x check-parens
M-x emacs-lisp-byte-compile


显示错乱

;; 重新解析
M-x lisp-tree-refresh


内存不足

对于非常大的文件，建议先选中关键区域进行分析。

9. 扩展功能

添加自定义分析

(defun lisp-tree-analyze-variables (nodes)
  "分析变量使用"
  (let ((vars nil))
    (cl-labels ((analyze-node (node)
                  (when (and (eq (lisp-tree-node-type node) 'symbol)
                             (not (keywordp (lisp-tree-node-value node)))
                             (not (functionp (lisp-tree-node-value node))))
                    (push (lisp-tree-node-value node) vars))
                  (dolist (child (lisp-tree-node-children node))
                    (analyze-node child))))
      (dolist (node nodes)
        (analyze-node node)))
    vars))


这个Lisp语法树分析工具可以很好地展示Lisp代码的结构，帮助理解和调试复杂的Lisp程序，特别适合学习Lisp语言和调试代码结构。


//less version lisp tree
Elisp 树形打印实现详解

我将为您展示如何在Elisp中实现和读取树形结构的打印功能。

1. 基础树结构定义

;; 树节点结构定义
(defstruct treenode
  value
  children)

;; 创建新节点
(defun make-treenode (value)
  (make-treenode :value value :children '()))

;; 添加子节点
(defun treenode-add-child (node child)
  (setf (treenode-children node)
        (append (treenode-children node) (list child)))
  node)


2. 基本树形打印函数

;; 简单的树形打印
(defun print-tree-simple (node &optional indent)
  "简单打印树结构"
  (let ((indent (or indent "")))
    ;; 打印当前节点
    (message "%s%s" indent (treenode-value node))
    ;; 递归打印子节点
    (dolist (child (treenode-children node))
      (print-tree-simple child (concat indent "  ")))))

;; 增强的树形打印（带连接线）
(defun print-tree (node &optional prefix is-last)
  "打印树形结构，带连接线"
  (let* ((prefix (or prefix ""))
         (node-prefix (if is-last "└── " "├── "))
         (child-prefix (if is-last "    " "│   ")))
    
    ;; 打印当前节点
    (message "%s%s%s" prefix node-prefix (treenode-value node))
    
    ;; 递归打印子节点
    (let ((child-count (length (treenode-children node)))
          (child-index 0))
      (dolist (child (treenode-children node))
        (let ((is-last-child (= (incf child-index) child-count))
              (new-prefix (concat prefix child-prefix)))
          (print-tree child new-prefix is-last-child))))))

;; 将树转换为字符串格式
(defun tree-to-string (node &optional prefix is-last)
  "将树转换为字符串表示"
  (let* ((prefix (or prefix ""))
         (node-prefix (if is-last "└── " "├── "))
         (child-prefix (if is-last "    " "│   "))
         (result ""))
    
    ;; 添加当前节点
    (setq result (concat result
                         prefix node-prefix
                         (treenode-value node) "\n"))
    
    ;; 递归添加子节点
    (let ((child-count (length (treenode-children node)))
          (child-index 0))
      (dolist (child (treenode-children node))
        (let ((is-last-child (= (incf child-index) child-count))
              (new-prefix (concat prefix child-prefix)))
          (setq result
                (concat result
                        (tree-to-string child new-prefix is-last-child))))))
    
    result))


3. 多种格式打印支持

;; 多种格式的树形打印
(defun print-tree-formatted (node &optional format)
  "以指定格式打印树"
  (let ((format (or format 'tree)))
    (cl-case format
      ('tree (print-tree node))
      ('lisp (print-tree-lisp node))
      ('lines (print-tree-lines node))
      ('indent (print-tree-indented node))
      (t (print-tree node)))))

;; Lisp风格打印（S表达式）
(defun print-tree-lisp (node)
  "以Lisp S表达式格式打印树"
  (if (null (treenode-children node))
      (message "%s" (treenode-value node))
    (let ((children-str ""))
      (dolist (child (treenode-children node))
        (setq children-str
              (concat children-str " " (tree-to-lisp-string child))))
      (message "(%s%s)" (treenode-value node) children-str))))

(defun tree-to-lisp-string (node)
  "将树转换为Lisp字符串"
  (if (null (treenode-children node))
      (treenode-value node)
    (let ((children-str ""))
      (dolist (child (treenode-children node))
        (setq children-str
              (concat children-str " " (tree-to-lisp-string child))))
      (format "(%s%s)" (treenode-value node) children-str))))

;; 缩进格式打印
(defun print-tree-indented (node &optional depth)
  "缩进格式打印树"
  (let ((depth (or depth 0))
        (indent (make-string (* depth 2) ? )))
    (message "%s%s" indent (treenode-value node))
    (dolist (child (treenode-children node))
      (print-tree-indented child (1+ depth)))))

;; 行格式打印（路径形式）
(defun print-tree-lines (node &optional path)
  "以路径形式打印树"
  (let* ((current-path (if path (concat path "/" (treenode-value node))
                         (treenode-value node))))
    (message "%s" current-path)
    (dolist (child (treenode-children node))
      (print-tree-lines child current-path))))


4. 彩色树形打印

;; 带颜色的树形打印
(require 'color)

(defun print-tree-colored (node &optional prefix is-last depth)
  "彩色树形打印"
  (let* ((depth (or depth 0))
         (prefix (or prefix ""))
         (node-prefix (if is-last "└── " "├── "))
         (child-prefix (if is-last "    " "│   "))
         (colors '("#FF6B6B" "#4ECDC4" "#45B7D1" "#96CEB4" "#FFEAA7" "#DDA0DD" "#98D8C8"))
         (color (nth (mod depth (length colors)) colors)))
    
    ;; 打印当前节点（带颜色）
    (put-text-property
     0 (length (treenode-value node))
     'face `(:foreground ,color :weight bold)
     (treenode-value node))
    
    (message "%s%s%s" prefix node-prefix (treenode-value node))
    
    ;; 递归打印子节点
    (let ((child-count (length (treenode-children node)))
          (child-index 0))
      (dolist (child (treenode-children node))
        (let ((is-last-child (= (incf child-index) child-count))
              (new-prefix (concat prefix child-prefix)))
          (print-tree-colored child new-prefix is-last-child (1+ depth)))))))

;; 根据节点类型着色
(defun print-tree-by-type (node &optional prefix is-last)
  "根据节点类型着色打印"
  (let* ((prefix (or prefix ""))
         (node-prefix (if is-last "└── " "├── "))
         (child-prefix (if is-last "    " "│   "))
         (value (treenode-value node))
         (face (cond
                ((string-match "\\." value) 'font-lock-string-face)  ; 文件
                ((string-match "^[A-Z]" value) 'font-lock-type-face) ; 类型
                (t 'font-lock-function-name-face)))) ; 目录
    
    ;; 应用face
    (put-text-property 0 (length value) 'face face value)
    
    (message "%s%s%s" prefix node-prefix value)
    
    (let ((child-count (length (treenode-children node)))
          (child-index 0))
      (dolist (child (treenode-children node))
        (let ((is-last-child (= (incf child-index) child-count))
              (new-prefix (concat prefix child-prefix)))
          (print-tree-by-type child new-prefix is-last-child))))))


5. 树形结构读取和解析

;; 从列表构建树
(defun list-to-tree (lst)
  "从嵌套列表构建树"
  (if (null lst)
      nil
    (let ((node (make-treenode (car lst))))
      (dolist (child (cdr lst))
        (if (listp child)
            (treenode-add-child node (list-to-tree child))
          (treenode-add-child node (make-treenode child))))
      node)))

;; 从目录结构构建树
(defun directory-to-tree (dir)
  "从目录结构构建树"
  (let ((node (make-treenode (file-name-nondirectory (directory-file-name dir)))))
    (dolist (file (directory-files dir t nil t))
      (unless (or (string-match "/\\.\\.?$" file)
                  (not (file-readable-p file)))
        (if (file-directory-p file)
            (treenode-add-child node (directory-to-tree file))
          (treenode-add-child node (make-treenode (file-name-nondirectory file))))))
    node))

;; 从字符串解析树（简单格式）
(defun parse-tree-from-string (str)
  "从字符串解析树结构"
  (let ((lines (split-string str "\n" t))
        (stack '())
        (root nil))
    (dolist (line lines)
      (let* ((indent (/ (length (string-match "^ *" line)) 2))
             (value (string-trim line))
             (node (make-treenode value)))
        
        ;; 调整堆栈
        (setq stack (nthcdr indent stack))
        
        (if (null stack)
            (setq root node
                  stack (list node))
          (treenode-add-child (car stack) node)
          (setq stack (cons node stack)))))
    root))


6. 完整示例和使用

;; 创建示例树
(defun create-example-tree ()
  "创建示例树结构"
  (let ((root (make-treenode "project-root"))
        (src (make-treenode "src"))
        (tests (make-treenode "tests"))
        (docs (make-treenode "docs")))
    
    ;; 添加文件
    (treenode-add-child src (make-treenode "main.el"))
    (treenode-add-child src (make-treenode "utils.el"))
    (treenode-add-child src (make-treenode "config.el"))
    
    ;; src 的子目录
    (let ((lib (make-treenode "lib")))
      (treenode-add-child lib (make-treenode "helper.el"))
      (treenode-add-child src lib))
    
    ;; tests
    (treenode-add-child tests (make-treenode "test-main.el"))
    (treenode-add-child tests (make-treenode "test-utils.el"))
    
    ;; docs
    (treenode-add-child docs (make-treenode "README.md"))
    (treenode-add-child docs (make-treenode "API.md"))
    
    ;; 构建树
    (treenode-add-child root src)
    (treenode-add-child root tests)
    (treenode-add-child root docs)
    
    root))

;; 演示函数
(defun demo-tree-printing ()
  "演示树形打印的各种方法"
  (interactive)
  (let ((tree (create-example-tree)))
    
    (message "\n=== 简单树形打印 ===")
    (print-tree-simple tree)
    
    (message "\n=== 带连接线的树形打印 ===")
    (print-tree tree)
    
    (message "\n=== 字符串格式的树 ===")
    (message "%s" (tree-to-string tree))
    
    (message "\n=== Lisp格式 (S表达式) ===")
    (print-tree-lisp tree)
    
    (message "\n=== 缩进格式 ===")
    (print-tree-indented tree)
    
    (message "\n=== 路径格式 ===")
    (print-tree-lines tree)
    
    (message "\n=== 彩色打印 ===")
    (print-tree-colored tree)
    
    (message "\n=== 从列表构建树 ===")
    (let ((tree2 (list-to-tree
                  '("root" 
                    ("dir1" "file1.el" "file2.el")
                    ("dir2" 
                     ("subdir" "nested.el"))
                    "README.md"))))
      (print-tree tree2))
    
    (message "\n=== 从当前目录构建树 ===")
    (when (yes-or-no-p "从当前目录构建树? ")
      (let ((dir-tree (directory-to-tree default-directory)))
        (print-tree dir-tree)))))


7. 高级功能：自定义树形打印

;; 自定义打印函数
(defun print-tree-custom (node &optional prefix is-last
                                 node-formatter child-prefix-formatter)
  "自定义树形打印"
  (let* ((prefix (or prefix ""))
         (node-formatter (or node-formatter
                            (lambda (n p l) 
                              (format "%s%s%s" p 
                                      (if l "└── " "├── ") 
                                      (treenode-value n)))))
         (child-prefix-formatter (or child-prefix-formatter
                                     (lambda (p l) 
                                       (concat p (if l "    " "│   "))))))
    
    ;; 打印当前节点
    (message (funcall node-formatter node prefix is-last))
    
    ;; 递归打印子节点
    (let ((child-count (length (treenode-children node)))
          (child-index 0))
      (dolist (child (treenode-children node))
        (let ((is-last-child (= (incf child-index) child-count))
              (new-prefix (funcall child-prefix-formatter prefix is-last)))
          (print-tree-custom child new-prefix is-last-child
                             node-formatter child-prefix-formatter))))))

;; 使用自定义格式化
(defun demo-custom-printing ()
  "演示自定义打印"
  (interactive)
  (let ((tree (create-example-tree)))
    
    (message "\n=== 简单箭头格式 ===")
    (print-tree-custom 
     tree "" t
     (lambda (node prefix is-last)
       (format "%s%s> %s" prefix 
               (if is-last "`" "|") 
               (treenode-value node)))
     (lambda (prefix is-last)
       (concat prefix (if is-last "  " "| "))))
    
    (message "\n=== 带统计信息的格式 ===")
    (print-tree-custom
     tree "" t
     (lambda (node prefix is-last)
       (let ((child-count (length (treenode-children node))))
         (format "%s%s%s [%d child%s]" prefix
                 (if is-last "└── " "├── ")
                 (treenode-value node)
                 child-count
                 (if (= child-count 1) "" "ren"))))
     (lambda (prefix is-last)
       (concat prefix (if is-last "    " "│   "))))))


8. 将树输出到buffer

;; 在buffer中显示树
(defun display-tree-in-buffer (tree buffer-name)
  "在指定buffer中显示树"
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert (tree-to-string tree))
    (special-mode)
    (display-buffer (current-buffer))))

;; 交互式树查看器
(defun view-tree ()
  "交互式查看树"
  (interactive)
  (let ((tree (create-example-tree)))
    (display-tree-in-buffer tree "*Tree View*")))


使用方法

;; 加载上述代码后，可以这样使用：

;; 1. 创建树
(setq my-tree (create-example-tree))

;; 2. 打印树
(print-tree my-tree)

;; 3. 获取树字符串
(setq tree-str (tree-to-string my-tree))

;; 4. 在buffer中查看
(view-tree)

;; 5. 运行演示
(demo-tree-printing)
(demo-custom-printing)


输出示例

运行 (demo-tree-printing) 会输出：

=== 带连接线的树形打印 ===
└── project-root
    ├── src
    │   ├── main.el
    │   ├── utils.el
    │   ├── config.el
    │   └── lib
    │       └── helper.el
    ├── tests
    │   ├── test-main.el
    │   └── test-utils.el
    └── docs
        ├── README.md
        └── API.md

=== Lisp格式 (S表达式) ===
(project-root (src main.el utils.el config.el (lib helper.el)) (tests test-main.el test-utils.el) (docs README.md API.md))


这个实现提供了完整的树形打印功能，支持多种格式和自定义选项，可以方便地在Elisp中处理和显示树形数据结构。







//git
;;; ==========================================
;;; Git 显示优化配置
;;; ==========================================

;;; 1. Magit - Git 前端
(use-package magit
  :demand t
  :custom
  ;; 基本设置
  (magit-auto-revert-mode nil)
  (magit-diff-refine-hunk t)           ; 精细显示差异
  (magit-save-repository-buffers 'dontask)
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-status-sections-hook
   '(magit-insert-status-headers
     magit-insert-merge-log
     magit-insert-rebase-sequence
     magit-insert-am-sequence
     magit-insert-sequencer-sequence
     magit-insert-bisect-output
     magit-insert-bisect-rest
     magit-insert-bisect-log
     magit-insert-untracked-files
     magit-insert-unstaged-changes
     magit-insert-staged-changes
     magit-insert-stashes
     magit-insert-unpushed-to-pushremote
     magit-insert-unpushed-to-upstream-or-recent
     magit-insert-unpulled-from-pushremote
     magit-insert-unpulled-from-upstream))
  
  ;; 差异显示优化
  (magit-diff-highlight-hunk-body t)    ; 高亮 hunk 体
  (magit-diff-highlight-trailing t)     ; 高亮尾部空白
  (magit-diff-paint-whitespace t)       ; 绘制空白字符
  (magit-diff-highlight-indentation t)  ; 高亮缩进
  (magit-diff-show-lines-before-context 3) ; 显示上下文行数
  (magit-diff-hide-trailing-cr-characters t) ; 隐藏尾部回车
  
  ;; 界面优化
  (magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (magit-log-arguments '("--graph" "--color" "--decorate" "--oneline"))
  (magit-log-auto-more t)               ; 自动加载更多日志
  (magit-section-visibility-indicator nil) ; 简化章节指示器
  
  ;; 提交界面
  (magit-commit-show-diff t)            ; 提交时显示差异
  (magit-commit-extend-override-date t) ; 扩展提交时覆盖日期
  
  ;; 颜色设置
  (magit-diff-added-highlight-face 'magit-diff-added-highlight)
  (magit-diff-removed-highlight-face 'magit-diff-removed-highlight)
  (magit-diff-added-face 'magit-diff-added)
  (magit-diff-removed-face 'magit-diff-removed)
  (magit-diff-context-highlight-face 'magit-diff-context-highlight)
  (magit-diff-file-heading-highlight-face 'magit-diff-file-heading-highlight)
  (magit-diff-hunk-heading-highlight-face 'magit-diff-hunk-heading-highlight)
  (magit-diff-hunk-heading-face 'magit-diff-hunk-heading)
  (magit-diff-lines-heading-face 'magit-diff-lines-heading)
  (magit-section-heading-face 'magit-section-heading)
  (magit-section-highlight-face 'magit-section-highlight)
  (magit-branch-local-face 'magit-branch-local)
  (magit-branch-remote-face 'magit-branch-remote)
  (magit-tag-face 'magit-tag)
  (magit-hash-face 'magit-hash)
  (magit-log-author-face 'magit-log-author)
  (magit-log-date-face 'magit-log-date)
  (magit-log-graph-face 'magit-log-graph)
  (magit-section-secondary-heading-face 'magit-section-secondary-heading)
  (magit-diffstat-added-face 'magit-diffstat-added)
  (magit-diffstat-removed-face 'magit-diffstat-removed)
  
  :bind
  (("C-x g" . magit-status)
   ("C-c g g" . magit-status)
   ("C-c g l" . magit-log-current)
   ("C-c g f" . magit-file-dispatch)
   ("C-c g b" . magit-branch-and-checkout)
   ("C-c g c" . magit-commit-create)
   ("C-c g p" . magit-pull)
   ("C-c g P" . magit-push)
   ("C-c g d" . magit-diff)
   ("C-c g s" . magit-stage-file)
   ("C-c g u" . magit-unstage-file))
  
  :config
  ;; Magit 颜色主题配置
  (set-face-attribute 'magit-diff-added nil
                      :foreground (gold-color :green)
                      :background (color-lighten-name (gold-color :green) 85))
  (set-face-attribute 'magit-diff-added-highlight nil
                      :foreground (gold-color :green)
                      :background (color-lighten-name (gold-color :green) 80)
                      :weight 'bold)
  (set-face-attribute 'magit-diff-removed nil
                      :foreground (gold-color :blood-red)
                      :background (color-lighten-name (gold-color :blood-red) 85))
  (set-face-attribute 'magit-diff-removed-highlight nil
                      :foreground (gold-color :blood-red)
                      :background (color-lighten-name (gold-color :blood-red) 80)
                      :weight 'bold)
  (set-face-attribute 'magit-diff-context nil
                      :foreground (gold-color :milky-fg-alt)
                      :background (gold-color :night-bg))
  (set-face-attribute 'magit-diff-context-highlight nil
                      :foreground (gold-color :milky-fg)
                      :background (gold-color :night-bg-alt))
  (set-face-attribute 'magit-diff-hunk-heading nil
                      :foreground (gold-color :gold-primary)
                      :background (gold-color :org-code-bg)
                      :weight 'bold
                      :overline (gold-color :gold-primary)
                      :underline (gold-color :gold-primary))
  (set-face-attribute 'magit-diff-hunk-heading-highlight nil
                      :foreground (gold-color :gold-primary)
                      :background (color-lighten-name (gold-color :gold-primary) 80)
                      :weight 'bold)
  (set-face-attribute 'magit-diff-lines-heading nil
                      :foreground (gold-color :night-bg)
                      :background (gold-color :gold-primary)
                      :weight 'bold)
  (set-face-attribute 'magit-diff-file-heading nil
                      :foreground (gold-color :cyan)
                      :weight 'bold)
  (set-face-attribute 'magit-diff-file-heading-highlight nil
                      :foreground (gold-color :cyan)
                      :background (color-lighten-name (gold-color :cyan) 80)
                      :weight 'bold)
  (set-face-attribute 'magit-section-heading nil
                      :foreground (gold-color :gold-primary)
                      :weight 'bold
                      :height 1.1)
  (set-face-attribute 'magit-section-highlight nil
                      :background (gold-color :night-bg-alt))
  (set-face-attribute 'magit-section-secondary-heading nil
                      :foreground (gold-color :gold-secondary)
                      :weight 'bold)
  (set-face-attribute 'magit-branch-local nil
                      :foreground (gold-color :cyan))
  (set-face-attribute 'magit-branch-remote nil
                      :foreground (gold-color :green))
  (set-face-attribute 'magit-tag nil
                      :foreground (gold-color :orange))
  (set-face-attribute 'magit-hash nil
                      :foreground (gold-color :gray))
  (set-face-attribute 'magit-log-author nil
                      :foreground (gold-color :aux-purple))
  (set-face-attribute 'magit-log-date nil
                      :foreground (gold-color :gray))
  (set-face-attribute 'magit-log-graph nil
                      :foreground (gold-color :gray-light))
  (set-face-attribute 'magit-diffstat-added nil
                      :foreground (gold-color :green))
  (set-face-attribute 'magit-diffstat-removed nil
                      :foreground (gold-color :blood-red))
  (set-face-attribute 'magit-dimmed nil
                      :foreground (gold-color :gray))
  (set-face-attribute 'magit-header-line nil
                      :foreground (gold-color :gold-primary)
                      :background (gold-color :night-bg-alt)
                      :weight 'bold)
  (set-face-attribute 'magit-process-ok nil
                      :foreground (gold-color :green)
                      :weight 'bold)
  (set-face-attribute 'magit-process-ng nil
                      :foreground (gold-color :blood-red)
                      :weight 'bold)
  
  ;; 键绑定优化
  (define-key magit-status-mode-map (kbd "TAB") 'magit-section-toggle)
  (define-key magit-status-mode-map (kbd "S-TAB") 'magit-section-cycle)
  (define-key magit-status-mode-map (kbd "C-c C-c") 'magit-process-buffer)
  (define-key magit-status-mode-map (kbd "C-c C-k") 'magit-process-kill)
  
  ;; 状态缓冲区优化
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-status-refresh-hook '(magit-diff-update-hunk-buffer))
  
  ;; 自动刷新设置
  (setq magit-refresh-status-buffer nil)  ; 不自动刷新状态缓冲区
  
  ;; 日志视图优化
  (setq magit-log-section-commit-count 10)  ; 每次显示的提交数
  (setq magit-log-margin-spec '(t " %Y-%m-%d %H:%M " magit-log-margin-width t 18))
  
  ;; 差异视图优化
  (setq magit-diff-section-file-format 'relative)  ; 相对路径
  (setq magit-diff-unmarked-lines-keep 1000)       ; 保留未标记行数
  
  ;; 自定义 magit 状态刷新频率
  (run-with-idle-timer 300 t (lambda () (when (derived-mode-p 'magit-status-mode)
                                         (magit-refresh)))))

;; Magit Popup
(use-package magit-popup
  :after magit
  :config
  (setq magit-popup-show-common-commands t))

;;; 2. Git Gutter - 行内 Git 状态
(use-package git-gutter
  :if (display-graphic-p)  ; 只在图形界面使用
  :hook ((prog-mode text-mode) . git-gutter-mode)
  :custom
  (git-gutter:modified-sign "┃")    ; 修改
  (git-gutter:added-sign "┃")       ; 新增
  (git-gutter:deleted-sign "┃")     ; 删除
  (git-gutter:unchanged-sign "")    ; 未修改
  (git-gutter:separator-sign "┃")   ; 分隔符
  (git-gutter:hide-gutter nil)      ; 不隐藏
  (git-gutter:window-width 1)       ; 窗口宽度
  (git-gutter:visual-line nil)      ; 不使用 visual-line
  (git-gutter:handled-backends '(git)) ; 只处理 git
  (git-gutter:update-interval 2)    ; 更新间隔
  (git-gutter:modified-sign "│")
  (git-gutter:added-sign "│")
  (git-gutter:deleted-sign "│")
  :config
  ;; 颜色设置
  (set-face-attribute 'git-gutter:modified nil
                      :foreground (gold-color :gold-primary)
                      :background (gold-color :night-bg))
  (set-face-attribute 'git-gutter:added nil
                      :foreground (gold-color :green)
                      :background (gold-color :night-bg))
  (set-face-attribute 'git-gutter:deleted nil
                      :foreground (gold-color :blood-red)
                      :background (gold-color :night-bg))
  (set-face-attribute 'git-gutter:separator nil
                      :foreground (gold-color :gray-light)
                      :background (gold-color :night-bg))
  
  ;; 快捷键
  (define-key git-gutter-mode-map (kbd "C-c g n") 'git-gutter:next-hunk)
  (define-key git-gutter-mode-map (kbd "C-c g p") 'git-gutter:previous-hunk)
  (define-key git-gutter-mode-map (kbd "C-c g s") 'git-gutter:stage-hunk)
  (define-key git-gutter-mode-map (kbd "C-c g r") 'git-gutter:revert-hunk)
  (define-key git-gutter-mode-map (kbd "C-c g v") 'git-gutter:popup-hunk)
  (define-key git-gutter-mode-map (kbd "C-c g u") 'git-gutter:update-all-windows)
  
  ;; 在文件保存时更新
  (add-hook 'after-save-hook 'git-gutter)
  
  ;; 自动隐藏空行
  (setq git-gutter:hide-gutter t)
  (setq git-gutter:diff-option "-w")
  
  ;; 性能优化
  (setq git-gutter:ask-p nil)
  (setq git-gutter:disabled-modes '(org-mode markdown-mode))
  
  ;; 增强的差异显示
  (git-gutter:linum-setup))

;;; 3. Diff Hunk - 改进的差异显示
(use-package diff-hl
  :if (display-graphic-p)
  :hook
  ((prog-mode text-mode conf-mode) . diff-hl-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-side 'left)                ; 显示在左边
  (diff-hl-draw-borders nil)          ; 不绘制边框
  (diff-hl-flydiff-delay 0.5)         ; 飞行动态延迟
  (diff-hl-margin-symbols-alist
   '((insert . "┃")                  ; 插入
     (delete . "┃")                  ; 删除
     (change . "┃")                  ; 修改
     (unknown . "?")                 ; 未知
     (ignored . "!")                 ; 忽略
     ))
  :config
  ;; 颜色设置
  (set-face-attribute 'diff-hl-change nil
                      :foreground (gold-color :gold-primary)
                      :background (gold-color :night-bg))
  (set-face-attribute 'diff-hl-insert nil
                      :foreground (gold-color :green)
                      :background (gold-color :night-bg))
  (set-face-attribute 'diff-hl-delete nil
                      :foreground (gold-color :blood-red)
                      :background (gold-color :night-bg))
  
  ;; 在 Dired 中启用
  (diff-hl-dired-mode)
  
  ;; 在 Magit 中显示
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  
  ;; 性能优化
  (setq diff-hl-disable-on-remote t)  ; 远程文件禁用
  (setq diff-hl-show-staged-changes nil) ; 不显示已暂存的更改
  
  ;; 快捷键
  (define-key diff-hl-mode-map (kbd "C-c g n") 'diff-hl-next-hunk)
  (define-key diff-hl-mode-map (kbd "C-c g p") 'diff-hl-previous-hunk)
  (define-key diff-hl-mode-map (kbd "C-c g s") 'diff-hl-stage-current-hunk)
  (define-key diff-hl-mode-map (kbd "C-c g r") 'diff-hl-revert-hunk)
  (define-key diff-hl-mode-map (kbd "C-c g v") 'diff-hl-show-hunk))

;;; 4. Git 时间线
(use-package git-timemachine
  :bind
  (("C-c g t" . git-timemachine)
   ("C-c g T" . git-timemachine-switch-branch))
  :config
  (set-face-attribute 'git-timemachine-commit nil
                      :foreground (gold-color :gold-primary)
                      :weight 'bold)
  (set-face-attribute 'git-timemachine-minibuffer-author-face nil
                      :foreground (gold-color :aux-purple))
  (set-face-attribute 'git-timemachine-minibuffer-detail-face nil
                      :foreground (gold-color :gray))
  
  ;; 快捷键
  (define-key git-timemachine-mode-map (kbd "n") 'git-timemachine-show-next-revision)
  (define-key git-timemachine-mode-map (kbd "p") 'git-timemachine-show-previous-revision)
  (define-key git-timemachine-mode-map (kbd "g") 'git-timemachine-show-nth-revision)
  (define-key git-timemachine-mode-map (kbd "q") 'git-timemachine-quit)
  (define-key git-timemachine-mode-map (kbd "b") 'git-timemachine-blame)
  (define-key git-timemachine-mode-map (kbd "w") 'git-timemachine-kill-abbreviated-revision)
  (define-key git-timemachine-mode-map (kbd "W") 'git-timemachine-kill-revision)
  (define-key git-timemachine-mode-map (kbd "a") 'git-timemachine-show-author)
  (define-key git-timemachine-mode-map (kbd "d") 'git-timemachine-show-commit)
  (define-key git-timemachine-mode-map (kbd "s") 'git-timemachine-show-subject))

;;; 5. Git 链接
(use-package git-link
  :bind
  (("C-c g l" . git-link)
   ("C-c g L" . git-link-commit))
  :config
  (setq git-link-open-in-browser t)
  (setq git-link-use-commit t)
  (setq git-link-commit-always-use-remote t)
  (setq git-link-default-branch "main"))

;;; 6. Blame 模式
(use-package magit-blame
  :after magit
  :bind
  (("C-c g b" . magit-blame-addition)
   ("C-c g B" . magit-blame-echo))
  :config
  ;; 颜色设置
  (set-face-attribute 'magit-blame-highlight nil
                      :background (gold-color :night-bg-alt)
                      :foreground (gold-color :milky-fg))
  (set-face-attribute 'magit-blame-name nil
                      :foreground (gold-color :aux-purple)
                      :weight 'bold)
  (set-face-attribute 'magit-blame-date nil
                      :foreground (gold-color :gray))
  (set-face-attribute 'magit-blame-summary nil
                      :foreground (gold-color :gold-secondary))
  (set-face-attribute 'magit-blame-heading nil
                      :foreground (gold-color :gold-primary)
                      :weight 'bold)
  
  ;; 设置 blame 格式
  (setq magit-blame-style 'headings)
  (setq magit-blame-heading-format "%H %-20a %s")
  (setq magit-blame-time-format "%Y-%m-%d %H:%M")
  (setq magit-blame-read-only t))

;;; 7. 改进的 Git 差异显示
(use-package diff-mode
  :ensure nil
  :config
  ;; 设置差异颜色
  (set-face-attribute 'diff-added nil
                      :foreground (gold-color :green)
                      :background (color-lighten-name (gold-color :green) 90))
  (set-face-attribute 'diff-removed nil
                      :foreground (gold-color :blood-red)
                      :background (color-lighten-name (gold-color :blood-red) 90))
  (set-face-attribute 'diff-changed nil
                      :foreground (gold-color :gold-primary)
                      :background (color-lighten-name (gold-color :gold-primary) 90))
  (set-face-attribute 'diff-context nil
                      :foreground (gold-color :milky-fg-alt))
  (set-face-attribute 'diff-file-header nil
                      :foreground (gold-color :cyan)
                      :weight 'bold)
  (set-face-attribute 'diff-function nil
                      :foreground (gold-color :aux-purple))
  (set-face-attribute 'diff-header nil
                      :foreground (gold-color :gold-primary)
                      :weight 'bold)
  (set-face-attribute 'diff-hunk-header nil
                      :foreground (gold-color :gold-primary)
                      :background (color-lighten-name (gold-color :gold-primary) 85)
                      :weight 'bold)
  (set-face-attribute 'diff-index nil
                      :foreground (gold-color :gray))
  (set-face-attribute 'diff-indicator-added nil
                      :foreground (gold-color :green)
                      :weight 'bold)
  (set-face-attribute 'diff-indicator-changed nil
                      :foreground (gold-color :gold-primary)
                      :weight 'bold)
  (set-face-attribute 'diff-indicator-removed nil
                      :foreground (gold-color :blood-red)
                      :weight 'bold)
  (set-face-attribute 'diff-nonexistent nil
                      :foreground (gold-color :gray-light))
  (set-face-attribute 'diff-refine-added nil
                      :foreground (gold-color :green)
                      :background (color-lighten-name (gold-color :green) 80)
                      :weight 'bold)
  (set-face-attribute 'diff-refine-removed nil
                      :foreground (gold-color :blood-red)
                      :background (color-lighten-name (gold-color :blood-red) 80)
                      :weight 'bold)
  
  ;; 增强差异显示
  (setq diff-font-lock-prettify t)
  (setq diff-update-on-the-fly t)
  (setq diff-auto-refine-mode t)
  (setq diff-add-log-use-relative-names t)
  
  ;; 键绑定
  (define-key diff-mode-map (kbd "C-c C-c") 'diff-goto-source)
  (define-key diff-mode-map (kbd "C-c C-n") 'diff-hunk-next)
  (define-key diff-mode-map (kbd "C-c C-p") 'diff-hunk-prev)
  (define-key diff-mode-map (kbd "C-c C-a") 'diff-apply-hunk)
  (define-key diff-mode-map (kbd "C-c C-r") 'diff-reverse-direction)
  (define-key diff-mode-map (kbd "C-c C-v") 'diff-refine-hunk))

;;; 8. Git 项目管理
(use-package projectile
  :config
  (setq projectile-project-root-functions
        '(projectile-project-root
          projectile-root-top-down
          projectile-root-bottom-up
          projectile-root-top-down-recurring))
  (setq projectile-git-command "git ls-files -zco --exclude-standard")
  (setq projectile-git-submodule-command "git submodule --quiet foreach 'echo $path'")
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-use-git-grep t)
  (setq projectile-git-fd-args "--hidden --no-ignore --exclude .git")
  (setq projectile-generic-command "fd . -0 --type f --color=never"))

;;; 9. Git 相关辅助工具
(defun gold-git-show-commit-at-point ()
  "显示当前行对应的 Git 提交"
  (interactive)
  (let ((commit (magit-thing-at-point 'commit)))
    (if commit
        (magit-show-commit commit)
      (message "没有找到提交"))))

(defun gold-git-blame-line ()
  "显示当前行的 Git blame 信息"
  (interactive)
  (magit-blame-addition (point) (point)))

(defun gold-git-diff-staged ()
  "显示已暂存的更改"
  (interactive)
  (magit-diff-staged))

(defun gold-git-diff-unstaged ()
  "显示未暂存的更改"
  (interactive)
  (magit-diff-unstaged))

(defun gold-git-open-file-remote ()
  "在远程仓库中打开当前文件"
  (interactive)
  (git-link)
  (browse-url (car kill-ring)))

(defun gold-git-status-quick ()
  "快速显示 Git 状态"
  (interactive)
  (let ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
    (magit-status)))

;;; 10. 自定义 Git 状态指示器
(defun gold-git-branch-indicator ()
  "在模式行显示 Git 分支"
  (let ((branch (magit-get-current-branch)))
    (if branch
        (concat "  " branch)
      "")))

(defun gold-git-status-indicator ()
  "在模式行显示 Git 状态"
  (when (and (buffer-file-name)
             (magit-git-repo-p default-directory))
    (let* ((status (magit-git-string "status" "--porcelain"))
           (changed (length (split-string (or status "") "\n" t)))
           (branch (magit-get-current-branch)))
      (when branch
        (format "  %s%s" branch
                (if (> changed 0)
                    (format " (%d)" changed)
                  ""))))))

;; 添加到模式行
(setq mode-line-format
      (cons '(:eval (gold-git-status-indicator)) mode-line-format))

;;; 11. Git 快捷键定义
(defvar gold-git-keymap (make-sparse-keymap)
  "Git 相关快捷键")

(define-prefix-command 'gold-git-prefix)
(define-key global-map (kbd "C-c g") 'gold-git-prefix)

;; Magit
(define-key gold-git-keymap (kbd "s") 'magit-status)
(define-key gold-git-keymap (kbd "S") 'gold-git-status-quick)
(define-key gold-git-keymap (kbd "l") 'magit-log-current)
(define-key gold-git-keymap (kbd "L") 'magit-log-buffer-file)
(define-key gold-git-keymap (kbd "f") 'magit-file-dispatch)
(define-key gold-git-keymap (kbd "b") 'magit-branch)
(define-key gold-git-keymap (kbd "B") 'magit-blame-addition)
(define-key gold-git-keymap (kbd "c") 'magit-commit-create)
(define-key gold-git-keymap (kbd "C") 'magit-commit-amend)
(define-key gold-git-keymap (kbd "p") 'magit-pull)
(define-key gold-git-keymap (kbd "P") 'magit-push)
(define-key gold-git-keymap (kbd "d") 'magit-diff)
(define-key gold-git-keymap (kbd "D") 'magit-diff-buffer-file)
(define-key gold-git-keymap (kbd "u") 'magit-stage-file)
(define-key gold-git-keymap (kbd "U") 'magit-unstage-file)

;; 差异相关
(define-key gold-git-keymap (kbd "d s") 'gold-git-diff-staged)
(define-key gold-git-keymap (kbd "d u") 'gold-git-diff-unstaged)
(define-key gold-git-keymap (kbd "d f") 'magit-diff-buffer-file)
(define-key gold-git-keymap (kbd "d w") 'magit-diff-working-tree)
(define-key gold-git-keymap (kbd "d c") 'magit-diff-while-committing)

;; Git 工具
(define-key gold-git-keymap (kbd "g l") 'git-link)
(define-key gold-git-keymap (kbd "g L") 'git-link-commit)
(define-key gold-git-keymap (kbd "g t") 'git-timemachine)
(define-key gold-git-keymap (kbd "g T") 'git-timemachine-switch-branch)
(define-key gold-git-keymap (kbd "g o") 'gold-git-open-file-remote)
(define-key gold-git-keymap (kbd "g s") 'gold-git-show-commit-at-point)
(define-key gold-git-keymap (kbd "g b") 'gold-git-blame-line)

;; 文件操作
(define-key gold-git-keymap (kbd "f a") 'magit-stage-file)
(define-key gold-git-keymap (kbd "f u") 'magit-unstage-file)
(define-key gold-git-keymap (kbd "f d") 'magit-discard)
(define-key gold-git-keymap (kbd "f r") 'magit-file-rename)
(define-key gold-git-keymap (kbd "f D") 'magit-file-delete)
(define-key gold-git-keymap (kbd "f U") 'magit-file-untrack)

;; 分支操作
(define-key gold-git-keymap (kbd "b c") 'magit-branch-and-checkout)
(define-key gold-git-keymap (kbd "b n") 'magit-branch-create)
(define-key gold-git-keymap (kbd "b d") 'magit-branch-delete)
(define-key gold-git-keymap (kbd "b m") 'magit-branch-rename)
(define-key gold-git-keymap (kbd "b r") 'magit-branch-reset)

;; 暂存操作
(define-key gold-git-keymap (kbd "s a") 'magit-stage-file)
(define-key gold-git-keymap (kbd "s u") 'magit-unstage-file)
(define-key gold-git-keymap (kbd "s A") 'magit-stage-modified)
(define-key gold-git-keymap (kbd "s U") 'magit-unstage-all)
(define-key gold-git-keymap (kbd "s d") 'magit-discard)
(define-key gold-git-keymap (kbd "s D") 'magit-discard-changes)

;;; 12. Git 状态自动刷新
(defun gold-git-auto-refresh ()
  "自动刷新 Git 状态"
  (when (and (buffer-file-name)
             (file-exists-p (buffer-file-name))
             (magit-git-repo-p default-directory))
    (when (and git-gutter-mode
               (not (memq major-mode '(dired-mode magit-status-mode magit-log-mode))))
      (git-gutter))))

;; 设置自动刷新计时器
(run-with-timer 0 60 'gold-git-auto-refresh)

;;; 13. Git 缓冲区显示优化
(defun gold-git-buffer-display-hook ()
  "Git 缓冲区显示优化"
  (when (derived-mode-p 'magit-mode)
    (setq show-trailing-whitespace nil)
    (setq truncate-lines t)
    (setq word-wrap nil)
    (visual-line-mode -1)
    (hl-line-mode 1)))

(add-hook 'magit-mode-hook 'gold-git-buffer-display-hook)

;;; 14. 提供模式
(provide 'gold-git)

;;; 15. 初始化
(defun gold-git-init ()
  "Git 显示优化初始化"
  (message "Git 显示优化已加载")
  (when (display-graphic-p)
    (git-gutter-mode 1)
    (diff-hl-mode 1)))

(run-with-idle-timer 2 nil 'gold-git-init)


核心功能说明

1. Magit 增强

• 智能状态显示：优化的状态缓冲区布局

• 彩色差异：使用黄金主题颜色的差异显示

• 精细差异：可展开查看详细的修改内容

• 快速导航：改进的键盘快捷键

• 分支管理：直观的分支创建、切换、合并

2. Git Gutter

• 行内状态指示：在行号区域显示修改状态

• 颜色编码：

  ◦ 绿色：新增行

  ◦ 黄色：修改行

  ◦ 红色：删除行

• 快速操作：可直接在行内暂存/恢复更改

• 性能优化：异步更新，不影响编辑

3. Diff Hunk

• 侧边栏显示：在窗口左侧显示修改状态


• 集成 Magit：在 Magit 中同步显示

• 多种符号：使用不同符号表示不同类型的修改

4. Git 时间线

• 历史浏览：浏览文件历史版本

• 对比查看：与任意历史版本对比

• 快速切换：在版本间快速切换

• 提交信息：显示完整的提交信息

5. Git 链接

• 远程链接：生成 GitHub/GitLab 链接

• 快速访问：一键在浏览器中打开

• 提交链接：生成特定提交的链接

• 文件链接：生成特定文件版本的链接

快捷键参考

主快捷键前缀：C-c g

Magit (C-c g)

• C-c g s - 状态

• C-c g S - 快速状态

• C-c g l - 当前日志

• C-c g L - 文件日志

• C-c g f - 文件操作

• C-c g b - 分支

• C-c g B - Blame

• C-c g c - 提交

• C-c g C - 修改提交

• C-c g p - 拉取

• C-c g P - 推送

• C-c g d - 差异

• C-c g D - 文件差异

• C-c g u - 暂存文件

• C-c g U - 取消暂存

差异 (C-c g d)

• C-c g d s - 已暂存的差异

• C-c g d u - 未暂存的差异

• C-c g d f - 文件差异

• C-c g d w - 工作树差异

• C-c g d c - 提交时的差异

Git 工具 (C-c g g)

• C-c g g l - 生成链接

• C-c g g L - 提交链接

• C-c g g t - 时间线

• C-c g g T - 切换分支时间线

• C-c g g o - 打开远程文件

• C-c g g s - 显示提交

• C-c g g b - 行级 Blame

文件操作 (C-c g f)

• C-c g f a - 暂存文件

• C-c g f u - 取消暂存

• C-c g f d - 丢弃

• C-c g f r - 重命名

• C-c g f D - 删除

• C-c g f U - 取消跟踪

分支操作 (C-c g b)

• C-c g b c - 创建并切换

• C-c g b n - 创建

• C-c g b d - 删除

• C-c g b m - 重命名

• C-c g b r - 重置

暂存操作 (C-c g s)

• C-c g s a - 暂存文件

• C-c g s u - 取消暂存

• C-c g s A - 暂存所有修改

• C-c g s U - 取消暂存所有

• C-c g s d - 丢弃

• C-c g s D - 丢弃所有更改

视觉优化

1. 颜色方案

• 新增：绿色（#50fa7b）

• 修改：黄金色（#ffd700）

• 删除：血红色（#ff5555）

• 分支：青色（#8be9fd）

• 远程：绿色（#50fa7b）

• 标签：橙色（#ffb86c）

2. 符号系统

• 修改标记：┃ 或 │

• 添加标记：┃ 或 │

• 删除标记：┃ 或 │

• 分隔符：┃

3. 布局优化

• 紧凑显示：减少空白区域

• 信息密度：在有限空间显示更多信息

• 视觉层次：清晰的视觉层次结构

• 响应式：根据窗口大小自动调整

性能优化

1. 更新策略

• 异步更新：不影响编辑体验

• 智能延迟：根据操作类型设置不同延迟

• 按需更新：只在需要时更新

• 缓存机制：减少重复计算

2. 资源管理

• 内存优化：及时释放不再需要的数据

• CPU 优化：避免不必要的计算

• IO 优化：减少磁盘访问

• 网络优化：智能处理远程操作

3. 用户体验

• 平滑动画：提供视觉反馈

• 快速响应：即时响应用户操作

• 错误处理：优雅的错误恢复

• 状态提示：清晰的操作状态

这个配置提供了完整的 Git 工作流支持，从基本的版本控制到高级的代码审查功能，全部集成了黄金主题的美学风格。
//layout
以下是一个完整的布局配置，实现您要求的三窗口布局、缓冲区显示规则、禁止窗口分割和 Ediff 布局管理：

;;; ==========================================
;;; 高级窗口布局与缓冲区管理配置
;;; ==========================================

;;; 1. 三窗口布局定义
(defvar gold-layout-active nil
  "黄金布局是否激活")

(defvar gold-original-window-config nil
  "保存原始窗口配置")

(defun gold-create-three-window-layout ()
  "创建三窗口布局：左上帮助，左下社区，右边主窗口（80列宽）"
  (interactive)
  (setq gold-layout-active t)
  (setq gold-original-window-config (current-window-configuration))
  
  ;; 先删除所有其他窗口，只保留一个
  (delete-other-windows)
  
  ;; 垂直分割，右边窗口80列宽
  (let ((main-window (selected-window)))
    (split-window-horizontally (- (window-width) 80))
    
    ;; 水平分割左边的窗口，上下各占一半
    (select-window (next-window))
    (split-window-vertically)
    
    ;; 现在有三个窗口
    (let ((top-left-window (selected-window))
          (bottom-left-window (next-window))
          (right-window (next-window (next-window))))
      
      ;; 命名窗口以便识别
      (set-window-parameter top-left-window 'gold-window-type 'help)
      (set-window-parameter bottom-left-window 'gold-window-type 'community)
      (set-window-parameter right-window 'gold-window-type 'main)
      
      ;; 设置窗口的专用缓冲区
      (set-window-dedicated-p top-left-window t)
      (set-window-dedicated-p bottom-left-window t)
      (set-window-dedicated-p right-window nil)  ; 主窗口不专用
      
      ;; 显示初始缓冲区
      (with-selected-window top-left-window
        (switch-to-buffer "*Help*")
        (gold-help-mode))
      
      (with-selected-window bottom-left-window
        (switch-to-buffer "*Community*")
        (gold-community-mode))
      
      (with-selected-window right-window
        (switch-to-buffer (current-buffer)))
      
      (message "三窗口布局已创建：左边帮助/社区，右边主窗口（80列）"))))

(defun gold-help-mode ()
  "帮助窗口模式"
  (setq mode-line-format '(" 帮助窗口 "))
  (setq cursor-type nil)
  (read-only-mode 1))

(defun gold-community-mode ()
  "社区窗口模式"
  (setq mode-line-format '(" 社区窗口 "))
  (setq cursor-type nil)
  (read-only-mode 1))

(defun gold-restore-layout ()
  "恢复黄金布局"
  (interactive)
  (when gold-layout-active
    (gold-create-three-window-layout)))

(defun gold-destroy-layout ()
  "销毁黄金布局"
  (interactive)
  (when gold-original-window-config
    (set-window-configuration gold-original-window-config)
    (setq gold-layout-active nil)
    (message "已恢复原始布局")))

;;; 2. 智能缓冲区显示规则
(defun gold-display-buffer-rule (buffer alist)
  "根据缓冲区名称决定显示位置"
  (let ((buffer-name (buffer-name buffer)))
    (cond
     ;; 显示在帮助窗口的缓冲区
     ((string-match-p "\\`\\(\\*Help\\|\\*Messages\\|\\*Warnings\\|\\*Compilation\\|\\*Backtrace\\|\\*Async Shell Command\\|\\*grep\\|\\*Occur\\|\\*Apropos\\|\\*Info\\|\\*Man\\|\\*ielm\\|\\*scratch\\|\\*eshell\\|\\*ielm\\|\\*ielm\\*\\)" buffer-name)
      (display-buffer-in-side-window
       buffer
       '((side . left)
         (slot . 0)           ; 顶部窗口
         (window-height . 0.4)
         (window-parameters . ((gold-window-type . help)
                               (no-delete-other-windows . t))))))
     
     ;; 显示在社区窗口的缓冲区
     ((string-match-p "\\`\\(\\*Community\\|\\*IRC\\|\\*ERC\\|\\*Matrix\\|\\*Discord\\|\\*Telegram\\|\\*Slack\\|\\*gnus\\|\\*mail\\|\\*notmuch\\|\\*mu4e\\|\\*elfeed\\|\\*Twitter\\|\\*Reddit\\|\\*Forum\\|\\*ChatGPT\\|\\*Copilot\\|\\*AI\\)" buffer-name)
      (display-buffer-in-side-window
       buffer
       '((side . left)
         (slot . 1)           ; 底部窗口
         (window-height . 0.6)
         (window-parameters . ((gold-window-type . community)
                               (no-delete-other-windows . t))))))
     
     ;; 其他情况显示在主窗口
     (t
      (display-buffer-in-side-window
       buffer
       '((side . right)
         (window-width . 80)
         (window-parameters . ((gold-window-type . main)))))))))

;; 配置 display-buffer-alist
(setq display-buffer-alist
      '(;; 帮助类缓冲区
        ("\\*Help\\*" (gold-display-buffer-rule))
        ("\\*Messages\\*" (gold-display-buffer-rule))
        ("\\*Warnings\\*" (gold-display-buffer-rule))
        ("\\*Compilation\\*" (gold-display-buffer-rule))
        ("\\*Backtrace\\*" (gold-display-buffer-rule))
        ("\\*Async Shell Command\\*" (gold-display-buffer-rule))
        ("\\*grep\\*" (gold-display-buffer-rule))
        ("\\*Occur\\*" (gold-display-buffer-rule))
        ("\\*Apropos\\*" (gold-display-buffer-rule))
        ("\\*Info\\*" (gold-display-buffer-rule))
        ("\\*Man.*\\*" (gold-display-buffer-rule))
        ("\\*ielm\\*" (gold-display-buffer-rule))
        ("\\*scratch\\*" (gold-display-buffer-rule))
        ("\\*eshell\\*" (gold-display-buffer-rule))
        
        ;; 社区/通信类缓冲区
        ("\\*Community\\*" (gold-display-buffer-rule))
        ("\\*IRC\\*" (gold-display-buffer-rule))
        ("\\*ERC\\*" (gold-display-buffer-rule))
        ("\\*Matrix\\*" (gold-display-buffer-rule))
        ("\\*Discord\\*" (gold-display-buffer-rule))
        ("\\*Telegram\\*" (gold-display-buffer-rule))
        ("\\*Slack\\*" (gold-display-buffer-rule))
        ("\\*gnus\\*" (gold-display-buffer-rule))
        ("\\*mail\\*" (gold-display-buffer-rule))
        ("\\*notmuch\\*" (gold-display-buffer-rule))
        ("\\*mu4e\\*" (gold-display-buffer-rule))
        ("\\*elfeed\\*" (gold-display-buffer-rule))
        ("\\*Twitter\\*" (gold-display-buffer-rule))
        ("\\*Reddit\\*" (gold-display-buffer-rule))
        ("\\*Forum\\*" (gold-display-buffer-rule))
        ("\\*ChatGPT\\*" (gold-display-buffer-rule))
        ("\\*Copilot\\*" (gold-display-buffer-rule))
        ("\\*AI\\*" (gold-display-buffer-rule))
        
        ;; 默认规则
        (".*" (gold-display-buffer-rule))))

;;; 3. 禁止自动分割窗口
(defun gold-advice-forbid-split-window (orig-fun &rest args)
  "禁止自动分割窗口的advice"
  (if (and gold-layout-active
           (not (memq this-command
                      '(gold-create-three-window-layout
                        gold-restore-layout
                        gold-ediff-layout
                        split-window-right
                        split-window-below
                        split-window-horizontally
                        split-window-vertically))))
      (let ((split-height-threshold nil)
            (split-width-threshold nil))
        (apply orig-fun args))
    (apply orig-fun args)))

;; 为相关函数添加advice
(dolist (func '(display-buffer
                pop-to-buffer
                switch-to-buffer
                other-window
                other-frame
                make-frame))
  (advice-add func :around #'gold-advice-forbid-split-window))

;;; 4. Ediff 布局管理
(defvar gold-ediff-original-layout nil
  "保存ediff之前的布局")

(defun gold-ediff-layout ()
  "设置ediff布局：左上A，右上B，底部操作窗口高10行"
  (interactive)
  (setq gold-ediff-original-layout (current-window-configuration))
  
  ;; 先确保我们有一个干净的布局
  (delete-other-windows)
  
  ;; 创建三个窗口
  (split-window-horizontally)  ; 左右分割
  (other-window 1)             ; 切换到右边窗口
  
  ;; 右边窗口再上下分割
  (split-window-vertically)
  (windmove-down)              ; 切换到右下窗口
  
  ;; 设置右下窗口高度为10行
  (window-resize (selected-window) (- 10 (window-height)) t)
  (windmove-up)                ; 回到右上窗口
  
  ;; 现在布局是：
  ;; 左上: buffer A
  ;; 右上: buffer B
  ;; 底部: 操作窗口 (10行高)
  
  (message "Ediff 布局已设置"))

(defun gold-ediff-restore-layout ()
  "恢复ediff之前的布局"
  (interactive)
  (when gold-ediff-original-layout
    (set-window-configuration gold-ediff-original-layout)
    (setq gold-ediff-original-layout nil)
    (message "已恢复 Ediff 前布局")))

;; Ediff 钩子
(defun gold-ediff-before-hook ()
  "Ediff 开始前的钩子"
  (gold-ediff-layout))

(defun gold-ediff-after-hook ()
  "Ediff 结束后的钩子"
  (gold-ediff-restore-layout))

(add-hook 'ediff-before-setup-hook #'gold-ediff-before-hook)
(add-hook 'ediff-quit-hook #'gold-ediff-after-hook)
(add-hook 'ediff-suspend-hook #'gold-ediff-after-hook)

;;; 5. 窗口管理函数
(defun gold-switch-to-window (type)
  "切换到指定类型的窗口"
  (interactive
   (list (completing-read "窗口类型: " '(help community main))))
  (let ((target-window nil))
    (walk-windows
     (lambda (window)
       (when (eq (window-parameter window 'gold-window-type)
                 (intern type))
         (setq target-window window))))
    (if target-window
        (select-window target-window)
      (message "未找到 %s 窗口" type))))

(defun gold-rotate-windows ()
  "在三个窗口间循环切换"
  (interactive)
  (let ((current-type (window-parameter (selected-window) 'gold-window-type)))
    (cond
     ((eq current-type 'help) (gold-switch-to-window "community"))
     ((eq current-type 'community) (gold-switch-to-window "main"))
     ((eq current-type 'main) (gold-switch-to-window "help"))
     (t (other-window 1)))))

(defun gold-resize-main-window (columns)
  "调整主窗口宽度"
  (interactive "n主窗口宽度（列数）: ")
  (when gold-layout-active
    (let ((main-window nil))
      (walk-windows
       (lambda (window)
         (when (eq (window-parameter window 'gold-window-type) 'main)
           (setq main-window window))))
      (when main-window
        (with-selected-window main-window
          (window-resize main-window (- columns (window-width)) t))
        (message "主窗口宽度已调整为 %d 列" columns)))))

;;; 6. 布局状态指示器
(defun gold-layout-status ()
  "返回布局状态字符串"
  (if gold-layout-active
      "黄金布局: 激活"
    "黄金布局: 未激活"))

(defun gold-window-info ()
  "显示当前窗口信息"
  (interactive)
  (let ((windows-info nil))
    (walk-windows
     (lambda (window)
       (push (format "窗口: %s, 类型: %s, 缓冲区: %s"
                     (window-number window)
                     (window-parameter window 'gold-window-type)
                     (buffer-name (window-buffer window)))
             windows-info)))
    (message "%s" (mapconcat 'identity windows-info "\n"))))

;;; 7. 快捷键绑定
(defvar gold-layout-keymap (make-sparse-keymap)
  "布局管理快捷键")

(define-prefix-command 'gold-layout-prefix)
(define-key global-map (kbd "C-c w") 'gold-layout-prefix)

;; 布局控制
(define-key gold-layout-keymap (kbd "c") 'gold-create-three-window-layout)
(define-key gold-layout-keymap (kbd "d") 'gold-destroy-layout)
(define-key gold-layout-keymap (kbd "r") 'gold-restore-layout)
(define-key gold-layout-keymap (kbd "s") 'gold-layout-status)
(define-key gold-layout-keymap (kbd "i") 'gold-window-info)

;; 窗口切换
(define-key gold-layout-keymap (kbd "h") (lambda () (interactive) (gold-switch-to-window "help")))
(define-key gold-layout-keymap (kbd "C") (lambda () (interactive) (gold-switch-to-window "community")))
(define-key gold-layout-keymap (kbd "m") (lambda () (interactive) (gold-switch-to-window "main")))
(define-key gold-layout-keymap (kbd "o") 'gold-rotate-windows)

;; 窗口调整
(define-key gold-layout-keymap (kbd "w") 'gold-resize-main-window)
(define-key gold-layout-keymap (kbd "=") 'balance-windows)
(define-key gold-layout-keymap (kbd "+") 'enlarge-window)
(define-key gold-layout-keymap (kbd "-") 'shrink-window)
(define-key gold-layout-keymap (kbd ">") 'enlarge-window-horizontally)
(define-key gold-layout-keymap (kbd "<") 'shrink-window-horizontally)

;; Ediff
(define-key gold-layout-keymap (kbd "e") 'gold-ediff-layout)
(define-key gold-layout-keymap (kbd "E") 'gold-ediff-restore-layout)

;;; 8. 布局持久化
(use-package desktop
  :config
  (setq desktop-path (list user-emacs-directory))
  (setq desktop-save t)
  (setq desktop-files-not-to-save "\\(^/tmp\\|/tmp\\)")
  (setq desktop-restore-eager 1)
  (setq desktop-auto-save-timeout 300)
  
  (defun gold-desktop-save-hook ()
    "保存桌面时保存布局状态"
    (when gold-layout-active
      (desktop-save-buffer 'gold-layout-state gold-layout-active)))
  
  (defun gold-desktop-restore-hook ()
    "恢复桌面时恢复布局状态"
    (when (and (boundp 'gold-layout-state) gold-layout-state)
      (gold-create-three-window-layout)))
  
  (add-hook 'desktop-save-hook 'gold-desktop-save-hook)
  (add-hook 'desktop-after-read-hook 'gold-desktop-restore-hook))

;;; 9. 缓冲区清理
(defun gold-cleanup-buffers ()
  "清理不需要的缓冲区"
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer)))
        (when (and (string-prefix-p "*" name)
                   (not (string-match-p "\\`\\*\\(Help\\|Community\\|Messages\\|Scratch\\)\\*" name))
                   (not (get-buffer-window buffer)))
          (kill-buffer buffer)))))
  (message "已清理不需要的缓冲区"))

(defun gold-refresh-layout-buffers ()
  "刷新布局中的缓冲区"
  (interactive)
  (when gold-layout-active
    (walk-windows
     (lambda (window)
       (let ((type (window-parameter window 'gold-window-type))
             (buffer (window-buffer window)))
         (with-selected-window window
           (cond
            ((eq type 'help)
             (switch-to-buffer "*Help*"))
            ((eq type 'community)
             (switch-to-buffer "*Community*"))
            ((and (eq type 'main)
                  (string-match-p "\\`\\*" (buffer-name buffer)))
             (switch-to-buffer (or (cl-loop for buf in (buffer-list)
                                            when (and (not (string-match-p "\\`\\*" (buffer-name buf)))
                                                      (not (eq buf buffer)))
                                            return buf)
                                   (current-buffer)))))))))
    (message "布局缓冲区已刷新")))

;;; 10. 自动布局管理
(defun gold-auto-manage-layout ()
  "自动管理布局"
  (when (and (display-graphic-p)
             (> (frame-width) 160))  ; 只有在足够宽的屏幕上才启用
    (unless gold-layout-active
      (gold-create-three-window-layout))))

;; 延迟执行自动布局
(run-with-idle-timer 2 nil #'gold-auto-manage-layout)

;; 窗口大小变化时调整布局
(defun gold-frame-resize-hook (frame)
  "窗口大小变化时的钩子"
  (when (eq frame (selected-frame))
    (when gold-layout-active
      (gold-resize-main-window 80))))

(add-hook 'window-size-change-functions #'gold-frame-resize-hook)

;;; 11. 布局主题集成
(defun gold-layout-theme-setup ()
  "布局主题设置"
  (when gold-layout-active
    ;; 设置窗口边框颜色
    (set-face-attribute 'window-divider nil
                        :foreground (gold-color :gray-light))
    (set-face-attribute 'vertical-border nil
                        :foreground (gold-color :gray-light))
    
    ;; 设置窗口模式线
    (walk-windows
     (lambda (window)
       (let ((type (window-parameter window 'gold-window-type)))
         (with-selected-window window
           (cond
            ((eq type 'help)
             (set-face-attribute 'mode-line nil
                                 :background (gold-color :aux-purple)
                                 :foreground (gold-color :night-bg)))
            ((eq type 'community)
             (set-face-attribute 'mode-line nil
                                 :background (gold-color :gold-primary)
                                 :foreground (gold-color :night-bg)))
            ((eq type 'main)
             (set-face-attribute 'mode-line nil
                                 :background (gold-color :night-bg-alt)
                                 :foreground (gold-color :milky-fg))))))))))

(add-hook 'gold-layout-active-hook 'gold-layout-theme-setup)

;;; 12. 提供模式
(provide 'gold-layout)

;;; 13. 初始化
(defun gold-layout-init ()
  "布局初始化"
  (message "黄金布局系统已加载")
  (gold-auto-manage-layout))

;; 延迟初始化
(run-with-idle-timer 3 nil #'gold-layout-init)


使用说明

1. 布局控制快捷键 (C-c w 前缀)

创建和管理布局

• C-c w c - 创建三窗口布局

• C-c w d - 销毁布局，恢复原始

• C-c w r - 恢复黄金布局

• C-c w s - 显示布局状态

• C-c w i - 显示窗口信息

窗口切换

• C-c w h - 切换到帮助窗口

• C-c w C - 切换到社区窗口

• C-c w m - 切换到主窗口

• C-c w o - 在三个窗口间循环切换

窗口调整

• C-c w w - 调整主窗口宽度

• C-c w = - 平衡窗口大小

• C-c w + - 增加窗口高度

• C-c w - - 减少窗口高度

• C-c w > - 增加窗口宽度

• C-c w < - 减少窗口宽度

Ediff 控制

• C-c w e - 设置 Ediff 布局

• C-c w E - 恢复 Ediff 前布局

2. 布局规则说明

三窗口布局

┌────────────────────┬────────────────────┐
│                    │                    │
│     帮助窗口       │                    │
│    (40%高度)       │                    │
├────────────────────┤     主窗口         │
│                    │     (80列宽)       │
│    社区窗口        │                    │
│    (60%高度)       │                    │
└────────────────────┴────────────────────┘


缓冲区显示规则

• 帮助窗口：*Help*, *Messages*, *Compilation*, *Warnings*, *grep*, *Occur*, *Info* 等

• 社区窗口：*IRC*, *ERC*, *gnus*, *mail*, *ChatGPT*, *AI* 等

• 主窗口：其他所有缓冲区

3. Ediff 布局

┌────────────────────┬────────────────────┐
│                    │                    │
│      Buffer A      │      Buffer B      │
│     (ediff)        │     (ediff)        │
├────────────────────┴────────────────────┤
│                                         │
│           操作窗口 (10行高)             │
│                                         │
└─────────────────────────────────────────┘


Ediff 会自动保存之前的布局，并在结束后恢复。

4. 自动功能

1. 自动布局：在宽屏显示器上自动启用三窗口布局

2. 窗口大小调整：窗口大小变化时自动调整布局

3. 布局持久化：保存和恢复桌面时保持布局状态

4. 缓冲区清理：可清理不需要的缓冲区

5. 配置选项

;; 自定义缓冲区显示规则
(add-to-list 'display-buffer-alist
             '("\\*Custom\\*" (gold-display-buffer-rule)))

;; 调整窗口比例
(setq gold-window-ratios '((help . 0.4) (community . 0.6)))

;; 调整主窗口宽度
(setq gold-main-window-width 100)  ; 设置为主窗口宽度


6. 故障排除

布局不显示

;; 检查布局状态
M-x gold-layout-status

;; 手动创建布局
M-x gold-create-three-window-layout


缓冲区显示在错误窗口

;; 检查缓冲区规则
M-x describe-variable display-buffer-alist

;; 手动移动缓冲区
M-x display-buffer 然后选择目标窗口


Ediff 布局不恢复

;; 手动恢复
M-x gold-ediff-restore-layout

;; 检查钩子
M-x describe-variable ediff-quit-hook


这个布局系统提供了智能的窗口管理，自动的缓冲区分类，以及优雅的 Ediff 集成，大大提高了多窗口工作流的效率。










//init
;;; ==========================================
;;; Emacs 初始化配置 - 性能与外观优化版
;;; ==========================================

;;; 0. 性能优化
(setq gc-cons-threshold 100000000)    ; 提高GC阈值
(setq read-process-output-max (* 1024 1024 4))
(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors nil)
(setq load-prefer-newer t)
(setq file-name-handler-alist nil)

;; 禁用启动时的杂项
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-buffer-menu t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)
(setq auto-save-default nil)          ; 禁用自动保存
(setq make-backup-files nil)          ; 禁用备份文件
(setq create-lockfiles nil)          ; 禁用锁文件
(setq ring-bell-function 'ignore)
(setq auto-save-list-file-prefix nil) ; 禁用自动保存列表文件

;;; 1. 包管理器
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-expand-minimally t)
(setq use-package-verbose nil)

;;; 2. 颜色系统
(defvar gold-theme-colors
  '((:night-bg      . "#0a0a0a")
    (:night-bg-alt  . "#1a1a1a")
    (:milky-fg      . "#f8f8f2")
    (:milky-fg-alt  . "#e2e2dc")
    (:gold-primary  . "#ffd700")
    (:gold-secondary . "#f1fa8c")
    (:blood-red     . "#ff5555")
    (:aux-purple    . "#bd93f9")
    (:aux-purple-alt . "#9370DB")
    (:cyan          . "#8be9fd")
    (:green         . "#50fa7b")
    (:orange        . "#ffb86c")
    (:pink          . "#ff79c6")
    (:gray          . "#6272a4")
    (:gray-light    . "#44475a")
    (:gray-dark     . "#2b2b2b")
    (:info-blue     . "#3399ff")
    (:success-green . "#00cc66")
    (:warning-orange . "#ff9900")
    (:error-red     . "#ff3333")
    (:type-blue     . "#66b2ff")
    (:func-yellow   . "#ffff99")
    (:const-cyan    . "#99ffff")
    (:string-green  . "#99ff99")
    (:org-code-bg   . "#2a2a00"))  ; 深黄金色背景
  "黄金主题颜色调色板")

(defun gold-color (name)
  "获取颜色值"
  (cdr (assoc name gold-theme-colors)))

;;; 3. 核心界面设置
(use-package emacs
  :custom
  ;; 全屏
  (default-frame-alist '((fullscreen . maximized)))
  
  ;; 禁用 GUI 元素
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (menu-bar-mode nil)
  
  ;; 窗口设置
  (frame-title-format "Emacs - %b")
  (column-number-mode t)
  (line-number-mode t))

;;; 4. 字体设置 - Maple Mono
(defun setup-fonts ()
  "设置字体为 Maple Mono"
  (when (display-graphic-p)
    ;; 主字体
    (set-face-attribute 'default nil
                        :family "Maple Mono"
                        :height 134
                        :weight 'normal)
    ;; 固定宽度字体
    (set-face-attribute 'fixed-pitch nil
                        :family "Maple Mono"
                        :height 134)
    ;; 可变宽度字体
    (set-face-attribute 'variable-pitch nil
                        :family "DejaVu Sans"
                        :height 134)
    ;; 行号字体
    (set-face-attribute 'line-number nil
                        :family "Maple Mono"
                        :height 124)
    (set-face-attribute 'line-number-current-line nil
                        :family "Maple Mono"
                        :height 124
                        :weight 'bold)
    (message "字体已设置为 Maple Mono 13.4")))

;;; 5. 黄金主题
(use-package gold-theme
  :load-path "~/.emacs.d/themes/"
  :demand t
  :config
  (load-theme 'gold-theme t))

;;; 6. 光标设置
(setq-default cursor-type 'underline)
(set-cursor-color (gold-color :blood-red))
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(setq blink-cursor-interval 0.5)

;;; 7. 基础增强
(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (set-face-attribute 'hl-line nil
                      :background (gold-color :night-bg-alt)
                      :underline (gold-color :blood-red)))

;;; 8. 行号设置
(use-package display-line-numbers
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative
        display-line-numbers-width 4
        display-line-numbers-grow-only t)
  (set-face-attribute 'line-number nil
                      :foreground (gold-color :gray)
                      :background (gold-color :night-bg))
  (set-face-attribute 'line-number-current-line nil
                      :foreground (gold-color :gold-primary)
                      :background (gold-color :night-bg-alt)
                      :weight 'bold))

;;; 9. 软换行设置
(setq-default truncate-lines nil)      ; 启用软换行
(setq word-wrap t)                     ; 在单词边界换行
(setq-default visual-line-mode t)      ; 启用 visual-line-mode
(global-visual-line-mode 1)            ; 全局启用软换行

;; 在某些模式下禁用软换行
(add-hook 'prog-mode-hook
          (lambda ()
            (setq truncate-lines t)    ; 编程模式下禁用软换行
            (visual-line-mode -1)))

;;; 10. Tab 和缩进设置
(setq-default tab-width 8)             ; Tab 宽度为 8
(setq-default indent-tabs-mode nil)    ; 使用空格而非 Tab
(setq tab-stop-list (number-sequence 8 120 8)) ; Tab 停止位置
(setq c-basic-offset 8)                ; C 风格语言缩进
(setq python-indent-offset 8)          ; Python 缩进
(setq js-indent-level 8)               ; JavaScript 缩进
(setq css-indent-offset 8)             ; CSS 缩进
(setq standard-indent 8)               ; 标准缩进

;; 显示空格和 Tab
(setq whitespace-style '(face tabs spaces trailing space-before-tab
                             newline indentation empty space-after-tab
                             space-mark tab-mark newline-mark))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])     ; 空格显示为中间点
        (newline-mark 10 [182 10])     ; 换行符
        (tab-mark 9 [9654 9] [92 9]))) ; Tab 显示为三角形
(global-whitespace-mode 1)             ; 启用全局空白显示

;;; 11. Undotree
(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory))))
  (setq undo-tree-visualizer-relative-timestamps t)
  (set-face-attribute 'undo-tree-visualizer-current-face nil
                      :foreground (gold-color :gold-primary)
                      :weight 'bold)
  (set-face-attribute 'undo-tree-visualizer-active-branch-face nil
                      :foreground (gold-color :aux-purple))
  (set-face-attribute 'undo-tree-visualizer-default-face nil
                      :foreground (gold-color :gray))
  :bind
  (("C-x u" . undo-tree-visualize)     ; 可视化 undo
   ("C-_" . undo-tree-undo)            ; 撤销
   ("M-_" . undo-tree-redo)))          ; 重做

;;; 12. 文件保存设置
(setq auto-save-default nil)           ; 禁用自动保存
(setq make-backup-files nil)           ; 禁用备份文件
(setq create-lockfiles nil)            ; 禁用锁文件
(setq auto-save-list-file-name nil)    ; 禁用自动保存列表
(setq version-control nil)             ; 不使用版本控制
(setq delete-old-versions t)           ; 删除旧版本
(setq kept-old-versions 0)             ; 不保留旧版本
(setq kept-new-versions 0)             ; 不保留新版本
(setq backup-directory-alist nil)      ; 无备份目录
(setq backup-by-copying nil)           ; 不复制备份
(setq vc-make-backup-files nil)        ; 版本控制下不备份

;; 保存时自动删除尾部空格
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; 13. Org Mode 配置
(use-package org
  :demand t
  :config
  ;; 基本设置
  (setq org-startup-indented t)        ; 启用缩进
  (setq org-adapt-indentation t)       ; 自适应缩进
  (setq org-src-preserve-indentation t) ; 保留源代码缩进
  (setq org-edit-src-content-indentation 0) ; 源代码内容缩进
  
  ;; 代码块设置
  (setq org-src-fontify-natively t)    ; 语法高亮
  (setq org-src-tab-acts-natively t)   ; Tab 键行为
  (setq org-src-window-setup 'current-window) ; 在当前窗口编辑
  
  ;; 代码块边框和背景色
  (set-face-attribute 'org-block-begin-line nil
                      :foreground (gold-color :gold-primary)
                      :background (gold-color :org-code-bg)
                      :overline (gold-color :gold-primary)
                      :underline (gold-color :gold-primary)
                      :weight 'bold)
  
  (set-face-attribute 'org-block-end-line nil
                      :foreground (gold-color :gold-primary)
                      :background (gold-color :org-code-bg)
                      :overline (gold-color :gold-primary)
                      :underline (gold-color :gold-primary)
                      :weight 'bold)
  
  (set-face-attribute 'org-block nil
                      :foreground (gold-color :milky-fg)
                      :background (gold-color :org-code-bg)
                      :extend t)
  
  (set-face-attribute 'org-code nil
                      :foreground (gold-color :gold-secondary)
                      :background (gold-color :org-code-bg))
  
  ;; 标题设置
  (set-face-attribute 'org-level-1 nil
                      :foreground (gold-color :gold-primary)
                      :height 1.3
                      :weight 'bold)
  (set-face-attribute 'org-level-2 nil
                      :foreground (gold-color :gold-secondary)
                      :height 1.2
                      :weight 'bold)
  (set-face-attribute 'org-level-3 nil
                      :foreground (gold-color :aux-purple)
                      :height 1.1)
  
  ;; 列表设置
  (set-face-attribute 'org-list-dt nil
                      :foreground (gold-color :cyan))
  
  ;; 链接设置
  (set-face-attribute 'org-link nil
                      :foreground (gold-color :cyan)
                      :underline t)
  
  ;; 待办事项
  (set-face-attribute 'org-todo nil
                      :foreground (gold-color :blood-red)
                      :weight 'bold)
  (set-face-attribute 'org-done nil
                      :foreground (gold-color :green)
                      :weight 'bold)
  
  ;; 元数据
  (set-face-attribute 'org-meta-line nil
                      :foreground (gold-color :gray))
  (set-face-attribute 'org-document-info-keyword nil
                      :foreground (gold-color :gray))
  
  ;; 日期
  (set-face-attribute 'org-date nil
                      :foreground (gold-color :pink)
                      :underline t)
  
  ;; 表格
  (set-face-attribute 'org-table nil
                      :foreground (gold-color :milky-fg-alt))
  
  ;; 引用
  (set-face-attribute 'org-quote nil
                      :foreground (gold-color :gold-secondary)
                      :slant 'italic
                      :extend t)
  
  ;; 文字修饰
  (set-face-attribute 'org-bold nil
                      :foreground (gold-color :gold-primary)
                      :weight 'bold)
  (set-face-attribute 'org-italic nil
                      :foreground (gold-color :aux-purple)
                      :slant 'italic)
  (set-face-attribute 'org-verbatim nil
                      :foreground (gold-color :gold-secondary)
                      :background (gold-color :org-code-bg))
  
  :bind
  (:map org-mode-map
        ("C-c C-c" . org-ctrl-c-ctrl-c)
        ("C-c C-e" . org-export-dispatch)
        ("C-c C-l" . org-insert-link)))

;;; 14. LSP 模式 - 性能优化
(use-package lsp-mode
  :demand t
  :commands (lsp lsp-deferred)
  :custom
  ;; 性能优化设置
  (lsp-enable-symbol-highlighting t)   ; 启用符号高亮
  (lsp-semantic-tokens-enable t)       ; 启用语义标记
  (lsp-enable-on-type-formatting nil)  ; 禁用输入时格式化（提高性能）
  (lsp-enable-text-document-color nil) ; 禁用文档颜色
  (lsp-enable-indentation nil)         ; 禁用缩进
  (lsp-enable-imenu t)                 ; 启用 imenu
  (lsp-enable-snippet t)               ; 启用代码片段
  (lsp-enable-file-watchers nil)       ; 禁用文件监视（提高性能）
  (lsp-file-watch-threshold nil)       ; 禁用文件监视阈值
  (lsp-lens-enable nil)                ; 禁用镜头
  (lsp-headerline-breadcrumb-enable nil) ; 禁用标题栏面包屑
  (lsp-modeline-diagnostics-enable t)  ; 启用诊断状态
  (lsp-modeline-code-actions-enable t) ; 启用代码操作状态
  (lsp-signature-auto-activate t)      ; 启用签名帮助
  (lsp-signature-render-documentation t) ; 渲染文档
  (lsp-completion-provider :none)      ; 禁用内置补全
  (lsp-idle-delay 0.5)                 ; 空闲延迟
  (lsp-log-io nil)                     ; 禁用日志
  (lsp-eldoc-enable-hover t)           ; 启用悬停
  (lsp-eldoc-render-all t)             ; 渲染所有
  
  ;; 字体锁定优化
  (lsp-semantic-tokens-apply-modifiers t) ; 应用修改器
  (lsp-semantic-tokens-apply-adjustments t) ; 应用调整
  
  :init
  (setq lsp-completion-enable t
        lsp-completion-show-detail t
        lsp-completion-show-kind t)
  
  :config
  ;; 字体锁定优化函数
  (defun optimize-lsp-font-lock ()
    "优化 LSP 字体锁定性能"
    (setq-local font-lock-maximum-decoration t)
    (setq-local font-lock-multiline t)
    (when (boundp 'jit-lock-mode)
      (jit-lock-mode 1))
    (setq-local lazy-highlight-cleanup nil)
    (setq-local lazy-highlight-initial-delay 0)
    (setq-local lazy-highlight-interval 0))
  
  (add-hook 'lsp-mode-hook 'optimize-lsp-font-lock)
  
  ;; 快捷键
  (define-key lsp-mode-map (kbd "C-c l d") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c l r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c l f") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c l a") 'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c l h") 'lsp-ui-doc-show)
  
  ;; 颜色配置
  (set-face-attribute 'lsp-face-highlight-textual nil
                      :background (gold-color :aux-purple)
                      :foreground (gold-color :night-bg))
  (set-face-attribute 'lsp-face-highlight-read nil
                      :background (gold-color :gold-primary)
                      :foreground (gold-color :night-bg))
  (set-face-attribute 'lsp-face-highlight-write nil
                      :background (gold-color :blood-red)
                      :foreground (gold-color :milky-fg)))

;; LSP UI
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-enable nil)            ; 禁用窥视（提高性能）
  (lsp-ui-sideline-enable nil)        ; 禁用侧边栏（提高性能）
  (lsp-ui-doc-enable t)               ; 启用文档
  (lsp-ui-doc-header t)               ; 启用文档头
  (lsp-ui-doc-include-signature t)    ; 包含签名
  (lsp-ui-doc-position 'top)          ; 文档位置
  (lsp-ui-doc-max-width 80)           ; 最大宽度
  (lsp-ui-doc-max-height 20)          ; 最大高度
  (lsp-ui-imenu-enable t)             ; 启用 imenu
  (lsp-ui-imenu-kind-position 'top)   ; imenu 位置
  :config
  (set-face-attribute 'lsp-ui-doc-background nil
                      :background (gold-color :night-bg-alt)
                      :foreground (gold-color :milky-fg))
  (set-face-attribute 'lsp-ui-doc-header nil
                      :background (gold-color :night-bg)
                      :foreground (gold-color :gold-primary)))

;;; 15. CORFU 补全系统
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-preselect-first t)
  (corfu-cycle t)
  (corfu-max-width 80)
  (corfu-min-width 30)
  (corfu-count 10)
  (corfu-scroll-margin 2)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET" . corfu-insert))
  :init
  (global-corfu-mode)
  :config
  (set-face-attribute 'corfu-current nil
                      :background (gold-color :aux-purple)
                      :foreground (gold-color :night-bg)
                      :weight 'bold)
  (set-face-attribute 'corfu-default nil
                      :background (gold-color :night-bg-alt)
                      :foreground (gold-color :milky-fg)))

;; Cape
(use-package cape
  :demand t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

;;; 16. 语法检查
(use-package flycheck
  :demand t
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (setq flycheck-idle-change-delay 2.0)
  (setq flycheck-display-errors-delay 0.2))

;;; 17. 项目管理
(use-package projectile
  :init
  (projectile-mode 1)
  :custom
  (projectile-completion-system 'default)
  (projectile-switch-project-action 'projectile-dired)
  (projectile-enable-caching t)
  (projectile-indexing-method 'native))

;;; 18. 搜索增强
(use-package consult
  :demand t
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-g g" . consult-goto-line)
   ("M-y" . consult-yank-pop)))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)))

;;; 19. 其他工具
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-colors
        (list (gold-color :gold-primary)
              (gold-color :aux-purple)
              (gold-color :blood-red)
              (gold-color :cyan)
              (gold-color :green))))

(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-minor-modes t
        doom-modeline-buffer-encoding t
        doom-modeline-icon t
        doom-modeline-time t))

;;; 20. 平滑滚动
(setq scroll-margin 5)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-frame t)

;;; 21. 括号匹配
(show-paren-mode 1)
(setq show-paren-style 'mixed
      show-paren-delay 0)
(set-face-attribute 'show-paren-match nil
                    :background (gold-color :aux-purple)
                    :foreground (gold-color :night-bg)
                    :weight 'bold)

;;; 22. 快捷键系统
(defvar gold-keymap (make-sparse-keymap)
  "黄金主题快捷键映射")

(define-prefix-command 'gold-prefix)
(define-key global-map (kbd "C-c g") 'gold-prefix)

;; 补全相关
(define-key gold-keymap (kbd "c TAB") 'completion-at-point)
(define-key gold-keymap (kbd "c c") 'corfu-complete)

;; LSP 相关
(define-key gold-keymap (kbd "l f") 'lsp-format-buffer)
(define-key gold-keymap (kbd "l r") 'lsp-rename)
(define-key gold-keymap (kbd "l d") 'lsp-find-definition)
(define-key gold-keymap (kbd "l h") 'lsp-ui-doc-show)

;; 文件操作
(define-key gold-keymap (kbd "f f") 'find-file)
(define-key gold-keymap (kbd "f b") 'consult-buffer)
(define-key gold-keymap (kbd "f s") 'save-buffer)
(define-key gold-keymap (kbd "f u") 'undo-tree-visualize)

;; 项目管理
(define-key gold-keymap (kbd "p s") 'projectile-switch-project)
(define-key gold-keymap (kbd "p f") 'projectile-find-file)

;; 搜索相关
(define-key gold-keymap (kbd "s s") 'consult-line)
(define-key gold-keymap (kbd "s b") 'consult-buffer)
(define-key gold-keymap (kbd "s g") 'consult-goto-line)

;;; 23. 最终初始化
(defun final-init ()
  "最终初始化函数"
  (setup-fonts)
  (setq gc-cons-threshold (* 100 1000 1000))
  
  (let ((init-time (float-time (time-subtract (current-time) before-init-time))))
    (message "🚀 Emacs 启动完成，耗时 %.2f 秒" init-time)
    
    (message "优化设置已生效:")
    (message "  • 字体: Maple Mono 13.4")
    (message "  • Tab 宽度: 8")
    (message "  • 行号: 相对行号")
    (message "  • 软换行: 已启用")
    (message "  • Undotree: 已启用")
    (message "  • 备份/自动保存: 已禁用")
    (message "  • Org 代码块: 黄金色边框")
    (message "  • LSP 字体锁定: 已优化")
    
    (message "使用 C-c g 查看所有快捷键")))

;; 记录启动时间
(defvar before-init-time (current-time))

;; 延迟执行最终初始化
(run-with-idle-timer 1 nil 'final-init)

;;; 24. 提供模式
(provide 'init)
;;; init.el ends here


配置说明

1. LSP 字体锁定性能优化

;; 禁用不必要的 LSP 功能以提高性能
(lsp-enable-on-type-formatting nil)  ; 禁用输入时格式化
(lsp-enable-file-watchers nil)       ; 禁用文件监视
(lsp-lens-enable nil)                ; 禁用镜头
(lsp-headerline-breadcrumb-enable nil) ; 禁用面包屑

;; 启用必要的优化
(lsp-semantic-tokens-enable t)       ; 语义标记
(lsp-semantic-tokens-apply-modifiers t) ; 应用修改器


2. 行号和软换行支持

;; 行号设置
(setq display-line-numbers-type 'relative)  ; 相对行号
(global-display-line-numbers-mode)         ; 全局启用

;; 软换行设置
(setq-default truncate-lines nil)          ; 启用软换行
(global-visual-line-mode 1)                ; 全局启用
;; 在编程模式下禁用软换行
(add-hook 'prog-mode-hook
          (lambda ()
            (setq truncate-lines t)
            (visual-line-mode -1)))


3. 默认字体：Maple Mono 13.4

(set-face-attribute 'default nil
                    :family "Maple Mono"
                    :height 134)  ; 13.4


4. Tab 宽度 8 空格

(setq-default tab-width 8)             ; Tab 宽度
(setq-default indent-tabs-mode nil)    ; 使用空格
(setq tab-stop-list (number-sequence 8 120 8)) ; Tab 停止位置
(setq c-basic-offset 8)                ; 所有语言缩进


5. Undotree 配置

(global-undo-tree-mode 1)              ; 全局启用
(setq undo-tree-auto-save-history t)   ; 自动保存历史
;; 快捷键
(global-set-key (kbd "C-x u") 'undo-tree-visualize)
(global-set-key (kbd "C-_") 'undo-tree-undo)
(global-set-key (kbd "M-_") 'undo-tree-redo)


6. 备份和自动保存设置

(setq auto-save-default nil)           ; 禁用自动保存
(setq make-backup-files nil)           ; 禁用备份
(setq create-lockfiles nil)            ; 禁用锁文件
(setq version-control nil)             ; 无版本控制


7. Org 代码块样式

;; 代码块边框和背景
(set-face-attribute 'org-block-begin-line nil
                    :foreground (gold-color :gold-primary)
                    :background (gold-color :org-code-bg)
                    :overline (gold-color :gold-primary)
                    :underline (gold-color :gold-primary))

(set-face-attribute 'org-block-end-line nil
                    :foreground (gold-color :gold-primary)
                    :background (gold-color :org-code-bg)
                    :overline (gold-color :gold-primary)
                    :underline (gold-color :gold-primary))

(set-face-attribute 'org-block nil
                    :background (gold-color :org-code-bg)
                    :extend t)


安装说明

1. 安装 Maple Mono 字体

# 从 GitHub 下载
cd ~/.local/share/fonts/
wget https://github.com/subframe7536/Maple-font/releases/download/v7.0-beta01/MapleMono-NF.zip
unzip MapleMono-NF.zip
fc-cache -fv


2. 安装 LSP 服务器

# Go
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest

# C/C++
sudo apt install clangd  # Ubuntu/Debian
brew install llvm        # macOS


3. 验证配置

;; 检查字体
M-x describe-font
;; 检查 Tab 宽度
M-x describe-variable tab-width
;; 检查 LSP
M-x lsp


这个配置提供了完整的优化体验，包括性能优化的 LSP、美观的界面、合理的编辑设置和高效的开发环境。

//utf-8

Emacs 对 UTF-8 有非常完善的支持。以下是关于在 Emacs 中使用 UTF-8 的关键信息：

检查当前编码

• 查看缓冲区编码：C-h C f RET

• 查看终端编码：M-x describe-terminal-coding-system

设置 UTF-8 编码

在配置文件（~/.emacs 或 ~/.emacs.d/init.el）中添加：

;; 优先使用 UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; 文件编码
(setq default-buffer-file-coding-system 'utf-8)
(modify-coding-system-alist 'file "\\.txt\\'" 'utf-8)


常用命令

• 查看/更改文件编码：C-x RET f

• 查看/更改缓冲区编码：C-x RET c

• 重新读取文件并转换编码：C-x RET r

• 保存时指定编码：C-x C-m c 或 C-x RET c

特殊场景

• 网络传输：(setq coding-system-for-read 'utf-8)

检测编码

如果打开文件时出现乱码，Emacs 会自动检测编码。也可以手动指定：

M-x revert-buffer-with-coding-system
Emacs 26+ 版本默认使用 UTF-8，通常无需额外配置。如果遇到编码问题，检查上述设置即可。
//font optimal
Emacs Font Lock 性能优化与检查指南

性能检查工具

1. 基本性能分析

;; 测量字体化时间
(benchmark-run 10
  (font-lock-fontify-buffer))

;; 检查当前 font-lock 设置
M-x describe-variable RET font-lock-keywords RET
M-x describe-variable RET font-lock-keywords-alist RET


2. 使用 profiler 分析

;; 启动性能分析
M-x profiler-start
M-x profiler-start cpu
M-x profiler-start memory

;; 进行一些操作（如打开大文件、滚动等）
;; 查看报告
M-x profiler-report
M-x profiler-stop


3. font-benchmark 包

;; 安装 font-benchmark
(use-package font-benchmark
  :ensure t
  :config
  (setq font-benchmark-directory "~/test-files"))


性能优化技巧

1. 调整 font-lock 级别

;; 降低字体化级别提高性能
(setq font-lock-maximum-decoration
      '((c-mode     . 2)    ; 级别 1-4，1 最快
        (c++-mode   . 2)
        (java-mode  . 2)
        (python-mode . 2)
        (emacs-lisp-mode . 3)))

;; 延迟字体化
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-defer-time 0.05)  ; 延迟 0.05 秒
(setq jit-lock-stealth-time 1)   ; 空闲 1 秒后字体化


2. 简化正则表达式

;; 避免过度复杂的正则表达式
(defun my-simplify-font-lock ()
  (font-lock-remove-keywords
   nil
   '(
     ;; 移除复杂的匹配
     ("\\(\\w+\\):\\(\\w+\\)" 1 'font-lock-type-face)
     )))


3. 使用 lazy 字体化

;; 只字体化可见区域
(setq font-lock-support-mode 'lazy-lock-mode)
(setq lazy-lock-defer-time 0.2)
(setq lazy-lock-defer-on-the-fly t)
(setq lazy-lock-defer-on-scrolling t)


选择性保留 font-lock

1. 保留特定类型的字体化

;; 只保留注释和字符串高亮
(defun my-minimal-font-lock ()
  (setq font-lock-defaults
        '(nil
          t
          nil
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-verbose . nil)))
  (font-lock-add-keywords
   nil
   '((";.*" . font-lock-comment-face)
     ("\".*\"" . font-lock-string-face)
     ("\\_<\\(defun\\|defvar\\|setq\\)\\_>" 1 font-lock-keyword-face))))


2. 按模式自定义

;; Python 模式：只保留基本语法
(add-hook 'python-mode-hook
          (lambda ()
            (setq font-lock-keywords
                  python-font-lock-keywords)
            (setq font-lock-keywords-only t)
            ;; 移除 docstring 高亮以提高性能
            (font-lock-remove-keywords
             nil
             '(("\\('''\\|\"\"\"\\).*\\1" 0 font-lock-doc-face t)))))

;; Org 模式优化
(add-hook 'org-mode-hook
          (lambda ()
            (setq org-fontify-done-headline nil)
            (setq org-hide-emphasis-markers t)))


3. 使用 font-lock-remove-keywords

;; 移除不需要的字体化
(defun my-remove-heavy-font-lock ()
  (interactive)
  ;; 移除复杂的高亮规则
  (font-lock-remove-keywords
   nil
   '(
     ;; 示例：移除复杂的正则匹配
     ("\\<\\(FIXME\\|TODO\\|NOTE\\):" 1 font-lock-warning-face t)
     ))
  (font-lock-flush)
  (font-lock-ensure))

;; 绑定到快捷键
(global-set-key (kbd "C-c f r") 'my-remove-heavy-font-lock)


性能监控命令

1. 检查当前字体化状态

;; 显示当前缓冲区字体化信息
(defun my-font-lock-info ()
  (interactive)
  (message "Font-lock keywords: %d"
           (length font-lock-keywords))
  (message "Font-lock mode: %s" font-lock-mode)
  (message "JIT lock mode: %s" jit-lock-mode))

;; 检查字体化缓存大小
M-x describe-variable RET jit-lock-chunk-size RET


2. 性能测试函数

(defun benchmark-font-lock ()
  "测试字体化性能"
  (interactive)
  (let ((start-time (current-time)))
    (font-lock-fontify-buffer)
    (message "字体化耗时: %.3f 秒"
             (float-time (time-since start-time)))))

(defun test-font-lock-performance ()
  "运行一系列性能测试"
  (interactive)
  ;; 测试 1: 基本字体化
  (message "=== 测试 1: 基本字体化 ===")
  (benchmark-font-lock)
  
  ;; 测试 2: 禁用字体化
  (message "\n=== 测试 2: 禁用字体化 ===")
  (let ((font-lock-mode nil))
    (benchmark-font-lock))
  
  ;; 测试 3: 不同级别
  (message "\n=== 测试 3: 不同装饰级别 ===")
  (dotimes (level 4)
    (let ((font-lock-maximum-decoration level))
      (message "级别 %d:" level)
      (benchmark-font-lock))))


实用配置建议

1. 针对大文件的优化

;; 大文件优化
(defun my-large-file-hook ()
  (when (> (buffer-size) 1000000)  ; 1MB 以上
    ;; 禁用部分高亮
    (setq font-lock-maximum-decoration 1)
    ;; 增大延迟
    (setq jit-lock-defer-time 0.5)
    ;; 禁用自动换行
    (setq truncate-lines t)
    ;; 减少语法检查
    (setq flycheck-check-syntax-automatically nil)))

(add-hook 'find-file-hook 'my-large-file-hook)


2. 模式特定的优化

;; 为不同编程语言设置不同的优化策略
(setq my-font-lock-optimizations
      '((c-mode . (:level 2 :delay 0.1))
        (python-mode . (:level 3 :delay 0.05))
        (web-mode . (:level 2 :delay 0.2))
        (emacs-lisp-mode . (:level 4 :delay 0.01))))

(defun apply-mode-optimizations ()
  (let* ((mode major-mode)
         (settings (cdr (assoc mode my-font-lock-optimizations))))
    (when settings
      (setq font-lock-maximum-decoration (plist-get settings :level))
      (setq jit-lock-defer-time (plist-get settings :delay)))))

(add-hook 'prog-mode-hook 'apply-mode-optimizations)


3. 动态调整策略

;; 根据系统负载动态调整
(defun dynamic-font-lock-adjust ()
  (cond ((> (car (load-average)) 2.0)
         ;; 高负载时降低质量
         (setq jit-lock-defer-time 0.3)
         (setq font-lock-maximum-decoration 1))
        ((< (car (load-average)) 0.5)
         ;; 低负载时提高质量
         (setq jit-lock-defer-time 0.01)
         (setq font-lock-maximum-decoration 3))
        (t
         ;; 中等负载
         (setq jit-lock-defer-time 0.1)
         (setq font-lock-maximum-decoration 2))))

(run-with-idle-timer 10 t 'dynamic-font-lock-adjust)


调试工具

1. 字体化调试命令

;; 查看哪些文本被字体化
M-x font-lock-debug-fontify

;; 重新字体化整个缓冲区
M-x font-lock-flush
M-x font-lock-ensure

;; 查看 font-lock 状态
M-x describe-font-lock


2. 内存使用检查

;; 查看内存使用
M-x memory-report

;; 检查缓冲区属性
M-x list-text-properties


最佳实践总结

1. 分级优化：根据文件大小和系统负载动态调整

2. 延迟字体化：使用 jit-lock-mode 和适当的延迟时间

3. 简化规则：移除不需要的复杂匹配规则

4. 按需启用：大文件时降低字体化级别

5. 定期监控：使用性能分析工具定期检查

通过合理配置，可以在保持语法高亮的同时获得良好的编辑性能。
逐行解释代码

;; 第1行：设置 font-lock 的支持模式为 lazy-lock-mode
(setq font-lock-support-mode 'lazy-lock-mode)


解释：

• setq：设置变量值的函数

• font-lock-support-mode：控制 Emacs 如何管理字体化（语法高亮）的变量

• 'lazy-lock-mode：使用 lazy lock 模式

• 作用：告诉 Emacs 使用惰性字体化策略，即只在需要时才进行语法高亮，而不是立即对整个缓冲区进行高亮，这对大文件性能有帮助

;; 第2行：设置 lazy lock 的延迟时间为 0.2 秒
(setq lazy-lock-defer-time 0.2)


解释：

• lazy-lock-defer-time：延迟字体化的时间（秒）

• 0.2：200 毫秒

• 作用：用户停止操作 0.2 秒后才开始字体化。例如，当您快速输入时，Emacs 会等待您暂停 0.2 秒后才进行语法高亮

;; 第3行：开启实时输入的延迟字体化
(setq lazy-lock-defer-on-the-fly t)


解释：

• lazy-lock-defer-on-the-fly：控制是否在用户输入时也使用延迟字体化

• t：true，启用

• 作用：即使在用户输入过程中也使用延迟策略。如果没有这个设置，可能在输入每个字符时都立即进行字体化

;; 第4行：开启滚动时的延迟字体化
(setq lazy-lock-defer-on-scrolling t)


解释：

• lazy-lock-defer-on-scrolling：控制是否在滚动时也使用延迟字体化

• t：true，启用

• 作用：在滚动时延迟字体化，直到停止滚动。这对于流畅滚动大型文件特别重要

;; 第5-11行：定义一个简化字体化的函数
(defun my-simplify-font-look ()
  (font-lock-remove-keywords
   nil
   '(
     ;; 移除复杂的匹配
     ("\\(\\w+\\):\\(\\w+\\)" 1 'font-lock-type-face)
     )))


详细解释：

;; 第5行：定义函数 my-simplify-font-look
(defun my-simplify-font-look ()


• defun：定义函数的关键字

• my-simplify-font-look：函数名

• ()：无参数

;; 第6行：调用 font-lock-remove-keywords 函数
  (font-lock-remove-keywords


• font-lock-remove-keywords：Emacs 内置函数，用于从字体化规则中移除特定的关键字匹配规则

;; 第7行：第一个参数 nil 表示当前缓冲区
   nil


• nil：表示操作应用于当前缓冲区

• 如果是模式名称（如 'python-mode），则应用于该模式的所有缓冲区

;; 第8-12行：要移除的字体化规则列表
   '(
     ;; 移除复杂的匹配
     ("\\(\\w+\\):\\(\\w+\\)" 1 'font-lock-type-face)
     )))


规则解释：

;; 第10行：一个字体化规则
("\\(\\w+\\):\\(\\w+\\)" 1 'font-lock-type-face)


• 正则表达式："\\(\\w+\\):\\(\\w+\\)"

  ◦ 匹配格式：单词:单词

  ◦ 例如：foo:bar、variable:value

• 捕获组 1：第一个 \\(\\w+\\) 部分

• 高亮面：'font-lock-type-face（类型的高亮样式）

• 作用：原本会将冒号前的单词高亮为类型（如变量类型）

整体功能总结

这段代码做了两件事：

1. 性能优化配置（前4行）

• 启用 lazy-lock 惰性字体化

• 设置 200 毫秒的延迟

• 开启输入和滚动时的延迟

• 目标：提高编辑大文件时的响应速度，避免每次按键都重新高亮整个文档

2. 简化字体化规则（函数定义）

• 定义了一个函数，从当前缓冲区移除特定的字体化规则

• 移除的规则是匹配 单词:单词 格式并将其高亮为类型

• 目标：移除可能影响性能的复杂正则表达式匹配

使用示例

;; 应用简化字体化
(my-simplify-font-look)

;; 或者绑定到钩子
(add-hook 'python-mode-hook 'my-simplify-font-look)
(add-hook 'js-mode-hook 'my-simplify-font-look)


注意

1. 这个函数只移除特定的字体化规则，不是全部规则

2. 移除后，类似 foo:bar 的文本将不再被高亮为类型

3. 如果要移除多个规则，可以在列表中添加更多：

(defun my-simplify-font-look ()
  (font-lock-remove-keywords
   nil
   '(
     ;; 移除多个复杂规则
     ("\\(\\w+\\):\\(\\w+\\)" 1 'font-lock-type-face)
     ("\\[\\(.*\\)\\]" 1 'font-lock-string-face)  ; 移除方括号内高亮
     ("#.*" 0 'font-lock-comment-face)            ; 移除 # 注释高亮
     )))

indent code

在 Emacs 中，代码块左侧的竖线（缩进指南）可以通过多种方式实现。以下是几种常见方法：

1. highlight-indent-guides 包（推荐）

这是一个流行的缩进指南插件。

;; 安装
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)  ; 或 'column, 'fill
  (setq highlight-indent-guides-character ?│)      ; 使用竖线字符
  (setq highlight-indent-guides-auto-enabled t)
  (setq highlight-indent-guides-responsive 'top))  ; 只在顶部显示


2. indent-guide 包

另一个选择：

(use-package indent-guide
  :ensure t
  :hook (prog-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "│")
  (setq indent-guide-recursive t))


3. Emacs 内置的 display-fill-column-indicator（Emacs 27+）

在指定列显示竖线（如 80 列）：

;; 显示 80 列边界线
(setq-default display-fill-column-indicator-column 80)
(global-display-fill-column-indicator-mode 1)

;; 自定义颜色
(set-face-foreground 'fill-column-indicator "#3f3f3f")


4. 通过 faces 自定义样式

自定义缩进指南外观：

;; highlight-indent-guides 的自定义
(set-face-background 'highlight-indent-guides-odd-face "gray20")
(set-face-background 'highlight-indent-guides-even-face "gray15")
(set-face-foreground 'highlight-indent-guides-character-face "gray40")

;; 修改竖线字符
(setq highlight-indent-guides-character ?┃)  ; 更粗的竖线
;; 或
(setq highlight-indent-guides-character ?▏)  ; 细竖线


5. 只针对特定模式

;; 只在特定模式启用
(add-hook 'python-mode-hook 'highlight-indent-guides-mode)
(add-hook 'js-mode-hook 'highlight-indent-guides-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-indent-guides-mode)


6. 简单的手动实现

如果不想安装包，可以用简单的函数：

;; 在左侧边缘显示竖线
(setq indicate-empty-lines t)
(setq indicate-buffer-boundaries 'left)

;; 显示行号时添加分隔线
(setq linum-format "%4d │ ")


常用字符选项

可用的竖线字符：

• ?│ - 细竖线

• ?┃ - 粗竖线

• ?┆ - 虚线竖线

• ?┊ - 更细的点线

• ?▏ - 1/8 宽竖线

• ?| - 管道符

我的推荐配置

(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?┆)  ; 虚线竖线
  (setq highlight-indent-guides-auto-character-face-perc 10)  ; 透明度
  (setq highlight-indent-guides-responsive 'top))


选择哪种方法取决于你的需求：

• highlight-indent-guides：功能最全，支持多种样式

• indent-guide：轻量级选择

• 内置功能：无需安装额外包

//indent














;; 方法1：设置制表符宽度为 8
(setq-default tab-width 8) ; 制表符显示宽度
;; 方法2：缩进宽度设置为 8 tab is spc

(setq-default standard-indent 8)
(setq-default indent-tabs-mode nil)  ; 使用空格缩进

;;nil or t tab show 8 spc
;; 确保 Tab 键插入制表符，而不是空格
;;(setq-default indent-tabs-mode t)




//curse
;; 简洁实用的配置
(setq-default cursor-type 'underline)
(set-cursor-color "#8be9fd")
(blink-cursor-mode 1)
(global-hl-line-mode 1)



//gold-theme
gold-theme.el - 黄金主题

以下是一个完整的 gold-theme.el 实现，完美融合了你要求的 血红、奶白、暗夜、黄金、辅助紫色 配色方案：

;;; gold-theme.el --- A luxurious dark theme with gold accents

;; Author: Your Name
;; Version: 1.0
;; URL: https://github.com/yourname/gold-theme
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:
;; A rich dark theme featuring:
;;   - Night black background (#0a0a0a)
;;   - Milky white foreground (#f8f8f2)
;;   - Gold accents (#ffd700, #f1fa8c)
;;   - Blood red highlights (#ff5555)
;;   - Auxiliary purple (#bd93f9)

;;; Code:

(deftheme gold-theme
  "A luxurious dark theme with gold accents and purple auxiliary colors")

(let ((class '((class color) (min-colors 89)))
      ;; Base colors
      (night-bg      "#0a0a0a")
      (night-bg-alt  "#1a1a1a")
      (milky-fg      "#f8f8f2")
      (milky-fg-alt  "#e2e2dc")
      
      ;; Accent colors
      (gold-primary  "#ffd700")
      (gold-secondary "#f1fa8c")
      (blood-red     "#ff5555")
      (aux-purple    "#bd93f9")
      (aux-purple-alt "#9370DB")
      
      ;; Additional colors
      (cyan          "#8be9fd")
      (green         "#50fa7b")
      (orange        "#ffb86c")
      (pink          "#ff79c6")
      (gray          "#6272a4")
      (gray-light    "#44475a"))

  (custom-theme-set-faces
   'gold-theme
   
   ;; ----- Basic faces -----
   `(default ((,class (:background ,night-bg :foreground ,milky-fg))))
   `(cursor ((,class (:background ,blood-red))))
   `(fringe ((,class (:background ,night-bg))))
   `(highlight ((,class (:background ,gray-light))))
   `(region ((,class (:background ,aux-purple :foreground ,night-bg))))
   `(secondary-selection ((,class (:background ,gray-light))))
   `(trailing-whitespace ((,class (:background ,blood-red))))
   `(vertical-border ((,class (:foreground ,gray-light))))
   
   ;; ----- Syntax highlighting -----
   `(font-lock-builtin-face ((,class (:foreground ,aux-purple))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,gray))))
   `(font-lock-comment-face ((,class (:foreground ,gray :italic t))))
   `(font-lock-constant-face ((,class (:foreground ,aux-purple))))
   `(font-lock-doc-face ((,class (:foreground ,gold-secondary))))
   `(font-lock-function-name-face ((,class (:foreground ,gold-primary :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,gold-primary :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,blood-red))))
   `(font-lock-preprocessor-face ((,class (:foreground ,aux-purple))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,gold-secondary))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,gold-secondary))))
   `(font-lock-string-face ((,class (:foreground ,gold-secondary))))
   `(font-lock-type-face ((,class (:foreground ,aux-purple))))
   `(font-lock-variable-name-face ((,class (:foreground ,milky-fg-alt))))
   `(font-lock-warning-face ((,class (:foreground ,blood-red :weight bold))))
   
   ;; ----- UI elements -----
   `(mode-line ((,class (:background ,night-bg-alt 
                                     :foreground ,aux-purple 
                                     :box (:line-width 3 :color ,night-bg-alt)))))
   `(mode-line-inactive ((,class (:background ,night-bg 
                                              :foreground ,gray 
                                              :box (:line-width 3 :color ,night-bg)))))
   `(mode-line-buffer-id ((,class (:foreground ,gold-primary :weight bold))))
   `(header-line ((,class (:inherit mode-line))))
   
   `(minibuffer-prompt ((,class (:foreground ,gold-primary :weight bold))))
   `(link ((,class (:foreground ,cyan :underline t))))
   `(link-visited ((,class (:foreground ,pink :underline t))))
   `(button ((,class (:foreground ,cyan :underline t))))
   `(widget-field ((,class (:background ,gray-light))))
   `(custom-button ((,class (:background ,night-bg-alt :foreground ,milky-fg))))
   `(custom-button-mouse ((,class (:background ,gray-light :foreground ,milky-fg))))
   `(custom-button-pressed ((,class (:background ,blood-red :foreground ,milky-fg))))
   
   `(isearch ((,class (:background ,gold-primary :foreground ,night-bg))))
   `(isearch-fail ((,class (:background ,blood-red :foreground ,milky-fg))))
   `(lazy-highlight ((,class (:background ,gold-secondary :foreground ,night-bg))))
   
   `(show-paren-match ((,class (:background ,aux-purple :foreground ,night-bg))))
   `(show-paren-mismatch ((,class (:background ,blood-red :foreground ,milky-fg))))
   
   ;; ----- Line numbers -----
   `(line-number ((,class (:foreground ,gray))))
   `(line-number-current-line ((,class (:foreground ,gold-primary :weight bold))))
   
   ;; ----- Current line highlight -----
   `(hl-line ((,class (:background ,night-bg-alt :underline ,blood-red))))
   
   ;; ----- Org-mode -----
   `(org-document-title ((,class (:foreground ,gold-primary :height 1.5 :weight bold))))
   `(org-level-1 ((,class (:foreground ,gold-primary :height 1.3 :weight bold))))
   `(org-level-2 ((,class (:foreground ,gold-secondary :height 1.2 :weight bold))))
   `(org-level-3 ((,class (:foreground ,aux-purple :height 1.1))))
   `(org-level-4 ((,class (:foreground ,cyan))))
   `(org-date ((,class (:foreground ,pink :underline t))))
   `(org-code ((,class (:foreground ,gold-secondary))))
   `(org-verbatim ((,class (:foreground ,gold-secondary))))
   `(org-todo ((,class (:foreground ,blood-red :weight bold))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(org-special-keyword ((,class (:foreground ,gray))))
   `(org-block ((,class (:background ,night-bg-alt))))
   `(org-block-begin-line ((,class (:background ,night-bg-alt :foreground ,gray))))
   `(org-block-end-line ((,class (:background ,night-bg-alt :foreground ,gray))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   
   ;; ----- Company -----
   `(company-tooltip ((,class (:background ,night-bg-alt :foreground ,milky-fg))))
   `(company-tooltip-common ((,class (:foreground ,gold-primary))))
   `(company-tooltip-selection ((,class (:background ,gray-light))))
   `(company-tooltip-annotation ((,class (:foreground ,gray))))
   `(company-scrollbar-bg ((,class (:background ,night-bg))))
   `(company-scrollbar-fg ((,class (:background ,aux-purple))))
   `(company-preview ((,class (:background ,blood-red :foreground ,milky-fg))))
   `(company-preview-common ((,class (:background ,blood-red :foreground ,gold-primary))))
   
   ;; ----- Ivy -----
   `(ivy-current-match ((,class (:background ,aux-purple :foreground ,night-bg))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,gray))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,gold-primary :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,gold-secondary :weight bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,aux-purple :weight bold))))
   `(ivy-virtual ((,class (:foreground ,gray))))
   
   ;; ----- Helm -----
   `(helm-source-header ((,class (:background ,night-bg-alt :foreground ,gold-primary :weight bold :height 1.2))))
   `(helm-selection ((,class (:background ,aux-purple :foreground ,night-bg))))
   `(helm-match ((,class (:foreground ,gold-primary :weight bold))))
   `(helm-candidate-number ((,class (:background ,night-bg-alt :foreground ,milky-fg))))
   `(helm-ff-directory ((,class (:foreground ,aux-purple :weight bold))))
   `(helm-ff-file ((,class (:foreground ,milky-fg))))
   `(helm-ff-executable ((,class (:foreground ,green))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,blood-red))))
   `(helm-ff-symlink ((,class (:foreground ,gold-primary))))
   `(helm-ff-prefix ((,class (:foreground ,blood-red))))
   
   ;; ----- Error/Warning -----
   `(error ((,class (:foreground ,blood-red :weight bold))))
   `(warning ((,class (:foreground ,orange :weight bold))))
   `(success ((,class (:foreground ,green :weight bold))))
   
   ;; ----- Diff -----
   `(diff-added ((,class (:foreground ,green))))
   `(diff-changed ((,class (:foreground ,gold-primary))))
   `(diff-removed ((,class (:foreground ,blood-red))))
   `(diff-header ((,class (:background ,night-bg-alt))))
   `(diff-file-header ((,class (:background ,night-bg-alt :foreground ,gold-primary :weight bold))))
   `(diff-hunk-header ((,class (:background ,night-bg-alt :foreground ,aux-purple))))
   
   ;; ----- Magit -----
   `(magit-section-heading ((,class (:foreground ,gold-primary :weight bold))))
   `(magit-branch-local ((,class (:foreground ,cyan))))
   `(magit-branch-remote ((,class (:foreground ,green))))
   `(magit-tag ((,class (:foreground ,gold-secondary))))
   `(magit-hash ((,class (:foreground ,gray))))
   `(magit-diff-added ((,class (:foreground ,green))))
   `(magit-diff-added-highlight ((,class (:background ,night-bg-alt :foreground ,green))))
   `(magit-diff-removed ((,class (:foreground ,blood-red))))
   `(magit-diff-removed-highlight ((,class (:background ,night-bg-alt :foreground ,blood-red))))
   `(magit-diff-context ((,class (:foreground ,milky-fg))))
   `(magit-diff-context-highlight ((,class (:background ,night-bg-alt))))
   `(magit-diff-hunk-heading ((,class (:background ,gray-light :foreground ,milky-fg))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,gray :foreground ,milky-fg))))
   `(magit-diff-lines-heading ((,class (:background ,blood-red :foreground ,milky-fg))))
   
   ;; ----- Terminal -----
   `(term-color-black ((,class (:background ,night-bg :foreground ,night-bg))))
   `(term-color-red ((,class (:background ,blood-red :foreground ,blood-red))))
   `(term-color-green ((,class (:background ,green :foreground ,green))))
   `(term-color-yellow ((,class (:background ,gold-primary :foreground ,gold-primary))))
   `(term-color-blue ((,class (:background ,aux-purple :foreground ,aux-purple))))
   `(term-color-magenta ((,class (:background ,pink :foreground ,pink))))
   `(term-color-cyan ((,class (:background ,cyan :foreground ,cyan))))
   `(term-color-white ((,class (:background ,milky-fg :foreground ,milky-fg))))
   
   ;; ----- Others -----
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,gold-primary))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,aux-purple))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,pink))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,gold-secondary))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,blood-red))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,gray))))
   
   `(highlight-indentation-face ((,class (:background ,night-bg-alt))))
   `(highlight-indentation-current-column-face ((,class (:background ,gray-light))))
   
   `(whitespace-space ((,class (:foreground ,gray))))
   `(whitespace-tab ((,class (:foreground ,gray))))
   `(whitespace-newline ((,class (:foreground ,gray))))
   `(whitespace-trailing ((,class (:background ,blood-red :foreground ,milky-fg))))
   
   `(eshell-prompt ((,class (:foreground ,gold-primary :weight bold))))
   `(eshell-ls-directory ((,class (:foreground ,aux-purple :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,green))))
   `(eshell-ls-symlink ((,class (:foreground ,gold-primary))))
   `(eshell-ls-backup ((,class (:foreground ,gray))))
   `(eshell-ls-product ((,class (:foreground ,gray))))
   `(eshell-ls-archive ((,class (:foreground ,orange))))
   `(eshell-ls-missing ((,class (:foreground ,blood-red))))
   `(eshell-ls-special ((,class (:foreground ,pink))))
   `(eshell-ls-readonly ((,class (:foreground ,gray))))
   `(eshell-ls-unreadable ((,class (:foreground ,gray))))
   `(eshell-ls-clutter ((,class (:foreground ,gray))))
   
   `(flycheck-error ((,class (:underline (:style wave :color ,blood-red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,orange)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,cyan)))))
   `(flyspell-incorrect ((,class (:underline (:style wave :color ,blood-red)))))
   `(flyspell-duplicate ((,class (:underline (:style wave :color ,orange)))))
   
   `(ansi-color-bold ((,class (:weight bold))))
   `(ansi-color-italic ((,class (:slant italic))))
   `(ansi-color-bright-white ((,class (:foreground ,milky-fg))))
   `(ansi-color-bright-red ((,class (:foreground ,blood-red))))
   `(ansi-color-bright-green ((,class (:foreground ,green))))
   `(ansi-color-bright-yellow ((,class (:foreground ,gold-primary))))
   `(ansi-color-bright-blue ((,class (:foreground ,aux-purple))))
   `(ansi-color-bright-magenta ((,class (:foreground ,pink))))
   `(ansi-color-bright-cyan ((,class (:foreground ,cyan))))
   `(ansi-color-bright-black ((,class (:foreground ,gray))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gold-theme)
(provide 'gold-theme)

;;; gold-theme.el ends here


安装与使用指南

1. 安装主题

1. 将上述代码保存为 gold-theme.el

2. 将文件放入Emacs的 custom-theme-load-path 目录（通常是 ~/.emacs.d/themes/）

3. 在 init.el 中添加：

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")


2. 启用主题

在Emacs中：

M-x load-theme RET gold-theme RET


或添加到配置中：

(load-theme 'gold-theme t)


3. 推荐配套设置

;; 启用行号
(global-display-line-numbers-mode 1)

;; 当前行高亮
(global-hl-line-mode 1)

;; 设置光标为血红色下划线
(setq-default cursor-type 'underline)
(set-cursor-color "#ff5555")

;; 启用彩虹分隔符
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 设置彩虹分隔符颜色
(setq rainbow-delimiters-colors
  '("#ffd700"    ; 黄金
    "#bd93f9"    ; 辅助紫色
    "#ff5555"    ; 血红
    "#8be9fd"    ; 青色
    "#50fa7b"))  ; 绿色


4. 主题特点

1. 核心配色:

  ◦ 暗夜背景: #0a0a0a

  ◦ 奶白前景: #f8f8f2

  ◦ 黄金主色: #ffd700

  ◦ 血红强调: #ff5555

  ◦ 辅助紫色: #bd93f9

2. 精心设计的语法高亮:

  ◦ 关键字: 黄金加粗

  ◦ 函数名: 黄金加粗

  ◦ 字符串: 柔和黄金

  ◦ 注释: 紫色斜体

  ◦ 错误: 血红加粗

3. 深度集成:

  ◦ 完美支持 Org-mode, Magit, Ivy/Helm, Company 等流行包

  ◦ 终端配色优化

  ◦ 行号、当前行高亮等细节处理

//weather
以下是支持三种选项的天气预报系统，支持异步获取，保持UTF-8编码和红色降雨概率提示：

;;; ==========================================
;;; 多城市天气预报系统 - 三种选项，异步获取
;;; ==========================================

(require 'url)
(require 'json)
(require 'org)
(require 'async)

;;; 1. 配置变量
(defcustom weather-api-provider 'open-meteo
  "天气API提供商，可选 'open-meteo 或 'openweather"
  :type '(choice (const :tag "Open-Meteo" open-meteo)
                 (const :tag "OpenWeather" openweather)
                 (const :tag "WeatherAPI" weatherapi))
  :group 'weather)

(defcustom weather-forecast-days 5
  "天气预报天数，可选 3 或 5 天"
  :type '(choice (const :tag "3天" 3)
                 (const :tag "5天" 5))
  :group 'weather)

(defcustom weather-units "metric"
  "温度单位，可选 'metric（摄氏度）或 'imperial（华氏度）"
  :type '(choice (const :tag "摄氏度" metric)
                 (const :tag "华氏度" imperial))
  :group 'weather)

(defcustom weather-show-icons t
  "是否显示天气图标"
  :type 'boolean
  :group 'weather)

;; API密钥配置
(defcustom openweather-api-key ""
  "OpenWeather API密钥，从 https://openweathermap.org/api 获取"
  :type 'string
  :group 'weather)

(defcustom weatherapi-api-key ""
  "WeatherAPI.com API密钥，从 https://www.weatherapi.com 获取"
  :type 'string
  :group 'weather)

;;; 2. 城市配置
(defvar weather-cities
  '(("北京"     "39.9042"   "116.4074")
    ("上海"     "31.2304"   "121.4737")
    ("广州"     "23.1291"   "113.2644")
    ("深圳"     "22.5431"   "114.0579")
    ("杭州"     "30.2741"   "120.1551")
    ("成都"     "30.5728"   "104.0668")
    ("南京"     "32.0603"   "118.7969")
    ("西安"     "34.3416"   "108.9398")
    ("武汉"     "30.5928"   "114.3055")
    ("重庆"     "29.5630"   "106.5516"))
  "城市列表：名称 纬度 经度")

;;; 3. 预设选项定义
(defun weather-set-option-a ()
  "设置选项a: 3天 WeatherAPI"
  (interactive)
  (setq weather-api-provider 'weatherapi)
  (setq weather-forecast-days 3)
  (message "已设置为选项a: 3天 WeatherAPI")
  (weather-quick-show))

(defun weather-set-option-b ()
  "设置选项b: 5天 OpenWeather"
  (interactive)
  (setq weather-api-provider 'openweather)
  (setq weather-forecast-days 5)
  (message "已设置为选项b: 5天 OpenWeather")
  (weather-quick-show))

(defun weather-set-option-c ()
  "设置选项c: 5天 Open-Meteo (默认)"
  (interactive)
  (setq weather-api-provider 'open-meteo)
  (setq weather-forecast-days 5)
  (message "已设置为选项c: 5天 Open-Meteo (默认)")
  (weather-quick-show))

;;; 4. 异步获取管理
(defvar weather-async-jobs nil
  "异步任务列表")

(defvar weather-async-results nil
  "异步获取结果")

(defvar weather-async-callback nil
  "异步完成回调函数")

;;; 5. API URL生成函数
(defun weather--get-api-url (lat lon)
  "根据选择的API提供商返回API URL"
  (cond
   ((eq weather-api-provider 'open-meteo)
    (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,precipitation_probability_max,weathercode&timezone=auto&forecast_days=%d"
            lat lon weather-forecast-days))
   
   ((eq weather-api-provider 'openweather)
    (if (string-empty-p openweather-api-key)
        (error "请先设置OpenWeather API密钥")
      (format "https://api.openweathermap.org/data/2.5/forecast?lat=%s&lon=%s&appid=%s&units=%s&cnt=%d"
              lat lon openweather-api-key weather-units
              (* weather-forecast-days 8))))  ; OpenWeather返回3小时间隔，每天8个数据点
   
   ((eq weather-api-provider 'weatherapi)
    (if (string-empty-p weatherapi-api-key)
        (error "请先设置WeatherAPI API密钥")
      (format "https://api.weatherapi.com/v1/forecast.json?key=%s&q=%s,%s&days=%d&aqi=no&alerts=no"
              weatherapi-api-key lat lon weather-forecast-days)))
   
   (t (error "未知的API提供商"))))

;;; 6. 数据解析函数
(defun weather--parse-open-meteo-data (json-data)
  "解析Open-Meteo返回的JSON数据"
  (let* ((daily (cdr (assoc 'daily json-data)))
         (time (cdr (assoc 'time daily)))
         (temp-max (cdr (assoc 'temperature_2m_max daily)))
         (temp-min (cdr (assoc 'temperature_2m_min daily)))
         (precip (cdr (assoc 'precipitation_sum daily)))
         (prob (cdr (assoc 'precipitation_probability_max daily)))
         (weathercode (cdr (assoc 'weathercode daily))))
    (cl-loop for i from 0 below (min (length time) weather-forecast-days)
             collect (list
                      (aref time i)
                      (aref temp-max i)
                      (aref temp-min i)
                      (aref precip i)
                      (aref prob i)
                      (aref weathercode i)))))

(defun weather--parse-openweather-data (json-data)
  "解析OpenWeather返回的JSON数据"
  (let* ((list-data (cdr (assoc 'list json-data)))
         (daily-data (make-hash-table :test 'equal))
         (days-collected 0))
    
    ;; 按日期分组
    (dolist (item list-data)
      (when (< days-collected weather-forecast-days)
        (let* ((dt (cdr (assoc 'dt item)))
               (date (format-time-string "%Y-%m-%d" (seconds-to-time dt)))
               (main (cdr (assoc 'main item)))
               (temp (cdr (assoc 'temp main)))
               (temp-min (cdr (assoc 'temp_min main)))
               (temp-max (cdr (assoc 'temp_max main)))
               (pop (cdr (assoc 'pop item)))  ; 降雨概率
               (weather (car (cdr (assoc 'weather item))))
               (weather-id (cdr (assoc 'id weather)))
               (rain (cdr (assoc 'rain item)))
               (rain-3h (if rain (cdr (assoc '3h rain)) 0.0)))
          
          (unless (gethash date daily-data)
            (puthash date (list date temp-max temp-min rain-3h (* 100 pop) weather-id) daily-data)
            (cl-incf days-collected))
          
          (let ((current (gethash date daily-data)))
            ;; 更新最高温和最低温
            (setf (nth 1 current) (max (nth 1 current) temp))
            (setf (nth 2 current) (min (nth 2 current) temp))
            ;; 更新降雨概率
            (setf (nth 4 current) (max (nth 4 current) (* 100 pop)))
            (puthash date current daily-data)))))
    
    ;; 转换为列表并按日期排序
    (let ((result nil))
      (maphash (lambda (key value) (push value result)) daily-data)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

(defun weather--parse-weatherapi-data (json-data)
  "解析WeatherAPI返回的JSON数据"
  (let* ((forecast (cdr (assoc 'forecast json-data)))
         (forecastday (cdr (assoc 'forecastday forecast)))
         (daily-data nil))
    
    (dotimes (i (min (length forecastday) weather-forecast-days))
      (let* ((day (aref forecastday i))
             (date (cdr (assoc 'date day)))
             (day-data (cdr (assoc 'day day)))
             (temp-max (cdr (assoc 'maxtemp_c day-data)))
             (temp-min (cdr (assoc 'mintemp_c day-data)))
             (precip (cdr (assoc 'totalprecip_mm day-data)))
             (prob (cdr (assoc 'daily_chance_of_rain day-data)))
             (condition (cdr (assoc 'condition day-data)))
             (weather-code (cdr (assoc 'code condition))))
        
        (push (list date temp-max temp-min precip prob weather-code) daily-data)))
    
    (reverse daily-data)))

(defun weather--get-weather-icon (weathercode)
  "根据天气代码返回对应的图标"
  (cond
   ((eq weather-api-provider 'open-meteo)
    (cond
     ((<= weathercode 1) "☀️")   ; 晴
     ((<= weathercode 2) "⛅")   ; 少云
     ((<= weathercode 3) "☁️")   ; 多云
     ((<= weathercode 48) "🌫️") ; 雾
     ((<= weathercode 55) "🌧️") ; 小雨
     ((<= weathercode 65) "🌧️") ; 中雨
     ((<= weathercode 75) "❄️")  ; 雪
     ((<= weathercode 77) "🌨️") ; 霰
     ((<= weathercode 82) "⛈️")  ; 雷暴
     ((<= weathercode 86) "🌨️") ; 雪
     ((<= weathercode 99) "⛈️")  ; 雷暴
     (t "❓")))
   
   ((eq weather-api-provider 'openweather)
    (cond
     ((<= weathercode 299) "⛈️")   ; 雷阵雨
     ((<= weathercode 399) "🌧️")   ; 细雨
     ((<= weathercode 499) "🌧️")   ; 中雨
     ((<= weathercode 599) "🌧️")   ; 大雨
     ((<= weathercode 699) "🌨️")   ; 雪
     ((<= weathercode 799) "🌫️")   ; 大气现象
     ((= weathercode 800) "☀️")     ; 晴
     ((<= weathercode 801) "⛅")    ; 少云
     ((<= weathercode 802) "☁️")    ; 散云
     ((<= weathercode 804) "☁️")    ; 阴天
     (t "❓")))
   
   ((eq weather-api-provider 'weatherapi)
    (cond
     ((= weathercode 1000) "☀️")   ; 晴
     ((= weathercode 1003) "⛅")   ; 部分多云
     ((= weathercode 1006) "☁️")   ; 多云
     ((= weathercode 1009) "☁️")   ; 阴天
     ((<= weathercode 1030) "🌫️") ; 雾
     ((<= weathercode 1063) "🌧️") ; 小雨
     ((<= weathercode 1180) "🌧️") ; 小雨
     ((<= weathercode 1186) "🌧️") ; 中雨
     ((<= weathercode 1192) "🌧️") ; 大雨
     ((<= weathercode 1201) "🌧️") ; 暴雨
     ((<= weathercode 1237) "🌨️") ; 冰雹
     ((<= weathercode 1264) "🌨️") ; 雨夹雪
     ((<= weathercode 1276) "⛈️")  ; 雷暴
     (t "❓")))
   
   (t "❓")))

;;; 7. 异步获取函数
(defun weather-async-fetch-city (city)
  "异步获取单个城市天气数据"
  (let* ((name (car city))
         (lat (cadr city))
         (lon (caddr city))
         (url (weather--get-api-url lat lon))
         (data nil))
    
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url t t 10)
          (goto-char (point-min))
          (when (search-forward-regexp "^$" nil t)
            (let ((json-data (json-read-from-string
                              (buffer-substring (point) (point-max)))))
              (kill-buffer (current-buffer))
              (setq data json-data))))
      (error
       (message "获取 %s 天气数据失败: %s" name (error-message-string err))
       nil))
    
    (when data
      (let ((parsed-data
             (cond
              ((eq weather-api-provider 'open-meteo)
               (weather--parse-open-meteo-data data))
              ((eq weather-api-provider 'openweather)
               (weather--parse-openweather-data data))
              ((eq weather-api-provider 'weatherapi)
               (weather--parse-weatherapi-data data))
              (t nil))))
        (when parsed-data
          (list name parsed-data))))))

(defun weather-async-fetch-cities (cities callback)
  "异步获取多个城市天气数据"
  (let ((results (make-hash-table :test 'equal))
        (total (length cities))
        (completed 0))
    
    (dolist (city cities)
      (async-start
       `(lambda ()
          (require 'url)
          (require 'json)
          (let ((result (weather-async-fetch-city ',city)))
            (cons ',(car city) result)))
       (lambda (result)
         (let ((city-name (car result))
               (city-data (cdr result)))
           (puthash city-name city-data results)
           (cl-incf completed)
           
           (message "已获取 %d/%d 个城市天气数据" completed total)
           
           (when (= completed total)
             ;; 按原始顺序整理结果
             (let ((ordered-results nil))
               (dolist (city cities)
                 (let ((city-name (car city))
                       (city-data (gethash city-name results)))
                   (when city-data
                     (push city-data ordered-results))))
               (setq ordered-results (reverse ordered-results))
               
               (funcall callback ordered-results)
               (message "所有城市天气数据获取完成！")))))))))

;;; 8. 显示函数
(defun weather-display-table (cities-data)
  "显示天气表格"
  (let ((buffer (get-buffer-create "*天气预报*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      
      ;; 标题
      (insert (format "#+TITLE: %d天天气预报 - 使用 %s\n" 
                      weather-forecast-days
                      (cond
                       ((eq weather-api-provider 'open-meteo) "Open-Meteo")
                       ((eq weather-api-provider 'openweather) "OpenWeather")
                       ((eq weather-api-provider 'weatherapi) "WeatherAPI")
                       (t "未知API"))))
      (insert (format "#+AUTHOR: Emacs Weather System (单位: %s)\n"
                      (if (string= weather-units "metric") "摄氏度" "华氏度")))
      (insert (format "#+DATE: %s\n" (format-time-string "%Y年%m月%d日")))
      (insert "#+OPTIONS: ^:nil\n")
      (insert "\n")
      
      ;; 创建表格
      (if weather-show-icons
          (insert "| 城市 | 日期 | 天气 | 最高温 | 最低温 | 降水量 | 降雨概率 |\n")
        (insert "| 城市 | 日期 | 最高温 | 最低温 | 降水量 | 降雨概率 |\n"))
      
      (if weather-show-icons
          (insert "|------+------+------+--------+--------+---------+----------|\n")
        (insert "|------+------+--------+--------+---------+----------|\n"))
      
      (dolist (city-data cities-data)
        (let ((city-name (car city-data))
              (daily-data (cdr city-data)))
          (dolist (day daily-data)
            (let* ((date (nth 0 day))
                   (temp-max (nth 1 day))
                   (temp-min (nth 2 day))
                   (precip (nth 3 day))
                   (prob (nth 4 day))
                   (weathercode (nth 5 day))
                   (icon (if weather-show-icons 
                             (weather--get-weather-icon weathercode) "")))
              
              (if weather-show-icons
                  (insert (format "| %s | %s | %s | " 
                                  city-name date icon))
                (insert (format "| %s | %s | " 
                                city-name date)))
              
              ;; 温度
              (insert (format "%s | %s | " 
                              (if temp-max 
                                  (format "%.1f°%s" temp-max
                                          (if (string= weather-units "metric") "C" "F"))
                                "N/A")
                              (if temp-min
                                  (format "%.1f°%s" temp-min
                                          (if (string= weather-units "metric") "C" "F"))
                                "N/A")))
              
              ;; 降水量
              (insert (format "%s | " 
                              (if precip (format "%.2f mm" precip) "0.00 mm")))
              
              ;; 降雨概率着色
              (if (and prob (>= prob 30))
                  (progn
                    (insert (propertize (format "%.0f%%" prob)
                                        'face '(:foreground "#ff5555" :weight bold)))
                    (insert " |\n"))
                (insert (format "%.0f%% |\n" (or prob 0))))))))
      
      (insert "\n")
      
      ;; 添加脚注
      (insert "#+BEGIN_COMMENT\n")
      (insert "* 说明：\n")
      (insert (format "1. 数据来源: %s\n" 
                      (cond
                       ((eq weather-api-provider 'open-meteo) "Open-Meteo API")
                       ((eq weather-api-provider 'openweather) "OpenWeather API")
                       ((eq weather-api-provider 'weatherapi) "WeatherAPI.com")
                       (t "未知")))
              (insert (format "2. 预报天数: %d 天\n" weather-forecast-days))
              (insert (format "3. 温度单位: %s\n" 
                              (if (string= weather-units "metric") "摄氏度" "华氏度")))
              (insert "4. 降雨概率 ≥ 30% 时显示为红色\n")
              (insert (format "5. 更新于: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
              
              (when (eq weather-api-provider 'openweather)
                (insert "6. OpenWeather 降水量为3小时累计值\n"))
              
              (insert "#+END_COMMENT\n")
              
              ;; 设置缓冲区属性
              (setq buffer-read-only t)
              (setq truncate-lines t)
              (visual-line-mode 1)
              (goto-char (point-min)))
      
      (display-buffer buffer))))

;;; 9. 主命令
(defun weather-show-multi-cities-async (city-count)
  "异步显示多个城市天气"
  (interactive "n显示几个城市的天气? (最大%d): ")
  (let* ((max-count (length weather-cities))
         (count (min city-count max-count))
         (cities (cl-subseq weather-cities 0 count)))
    
    (message "正在从 %s 异步获取 %d 个城市天气数据..." 
             (symbol-name weather-api-provider) count)
    
    (weather-async-fetch-cities
     cities
     (lambda (results)
       (if results
           (weather-display-table results)
         (message "获取天气数据失败，请检查网络连接或API配置。"))))))

(defun weather-show-all-cities-async ()
  "异步显示所有预设城市天气"
  (interactive)
  (weather-show-multi-cities-async (length weather-cities)))

;;; 10. 配置函数
(defun weather-set-provider (provider)
  "设置天气API提供商"
  (interactive
   (list (completing-read "选择API提供商: "
                          '("open-meteo" "openweather" "weatherapi")
                          nil t)))
  (setq weather-api-provider (intern provider))
  (message "已设置天气API提供商为: %s" provider))

(defun weather-set-forecast-days (days)
  "设置天气预报天数"
  (interactive
   (list (completing-read "选择预报天数: "
                          '("3" "5")
                          nil t)))
  (setq weather-forecast-days (string-to-number days))
  (message "已设置天气预报天数为: %d 天" weather-forecast-days))

(defun weather-set-openweather-key (key)
  "设置OpenWeather API密钥"
  (interactive "sOpenWeather API密钥: ")
  (setq openweather-api-key key)
  (message "已设置OpenWeather API密钥"))

(defun weather-set-weatherapi-key (key)
  "设置WeatherAPI API密钥"
  (interactive "sWeatherAPI.com API密钥: ")
  (setq weatherapi-api-key key)
  (message "已设置WeatherAPI.com API密钥"))

(defun weather-switch-units ()
  "切换温度单位"
  (interactive)
  (if (string= weather-units "metric")
      (setq weather-units "imperial")
    (setq weather-units "metric"))
  (message "已切换温度单位为: %s" 
           (if (string= weather-units "metric") "摄氏度" "华氏度")))

(defun weather-toggle-icons ()
  "切换是否显示天气图标"
  (interactive)
  (setq weather-show-icons (not weather-show-icons))
  (message "天气图标显示: %s" (if weather-show-icons "开启" "关闭")))

;;; 11. 快捷命令
(defun weather-quick-show ()
  "快速显示天气（异步）"
  (interactive)
  (weather-show-all-cities-async))

;;; 12. 选项菜单
(defun weather-show-options-menu ()
  "显示选项菜单"
  (interactive)
  (let ((buffer (get-buffer-create "*天气选项*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "#+TITLE: 天气预报选项\n")
      (insert "\n")
      
      (insert "* 当前设置\n")
      (insert (format "- API提供商: %s\n" 
                      (cond
                       ((eq weather-api-provider 'open-meteo) "Open-Meteo")
                       ((eq weather-api-provider 'openweather) "OpenWeather")
                       ((eq weather-api-provider 'weatherapi) "WeatherAPI")
                       (t "未知"))))
      (insert (format "- 预报天数: %d 天\n" weather-forecast-days))
      (insert (format "- 温度单位: %s\n" 
                      (if (string= weather-units "metric") "摄氏度" "华氏度")))
      (insert (format "- 显示图标: %s\n" (if weather-show-icons "是" "否")))
      
      (insert "\n* 预设选项\n")
      (insert "1. 选项a: 3天 WeatherAPI (需要API密钥)\n")
      (insert "2. 选项b: 5天 OpenWeather (需要API密钥)\n")
      (insert "3. 选项c: 5天 Open-Meteo (默认，无需API密钥)\n")
      
      (insert "\n* 使用方法\n")
      (insert "- 按数字键 1, 2, 3 选择预设选项\n")
      (insert "- 按字母键自定义设置:\n")
      (insert "  p - 选择API提供商\n")
      (insert "  d - 选择预报天数\n")
      (insert "  u - 切换温度单位\n")
      (insert "  i - 切换图标显示\n")
      (insert "  s - 显示天气\n")
      (insert "  q - 退出\n")
      
      (local-set-key (kbd "1") 'weather-set-option-a)
      (local-set-key (kbd "2") 'weather-set-option-b)
      (local-set-key (kbd "3") 'weather-set-option-c)
      (local-set-key (kbd "p") 'weather-set-provider)
      (local-set-key (kbd "d") 'weather-set-forecast-days)
      (local-set-key (kbd "u") 'weather-switch-units)
      (local-set-key (kbd "i") 'weather-toggle-icons)
      (local-set-key (kbd "s") 'weather-quick-show)
      (local-set-key (kbd "q") (lambda () (interactive) (bury-buffer)))
      
      (org-mode)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer buffer)))

;;; 13. 初始化
(defun weather-init ()
  "初始化天气系统"
  (message "天气系统已加载")
  (weather-check-api-availability))

(defun weather-check-api-availability ()
  "检查API可用性"
  (interactive)
  (message "正在检查 %s API可用性..." (symbol-name weather-api-provider))
  
  (cond
   ((and (eq weather-api-provider 'openweather)
         (string-empty-p openweather-api-key))
    (message "警告: OpenWeather需要API密钥，请先设置"))
   
   ((and (eq weather-api-provider 'weatherapi)
         (string-empty-p weatherapi-api-key))
    (message "警告: WeatherAPI需要API密钥，请先设置"))
   
   (t
    (async-start
     `(lambda ()
        (require 'url)
        (require 'json)
        (let ((test-city '("北京" "39.9042" "116.4074")))
          (weather-async-fetch-city test-city)))
     (lambda (result)
       (if result
           (message "%s API连接正常" (symbol-name weather-api-provider))
         (message "警告: 无法连接到 %s API" (symbol-name weather-api-provider))))))))

;; 延迟初始化
(run-with-idle-timer 2 nil 'weather-init)

;;; 14. 全局快捷键
(global-set-key (kbd "C-c w w") 'weather-quick-show)
(global-set-key (kbd "C-c w o") 'weather-show-options-menu)
(global-set-key (kbd "C-c w a") 'weather-set-option-a)
(global-set-key (kbd "C-c w b") 'weather-set-option-b)
(global-set-key (kbd "C-c w c") 'weather-set-option-c)

;;; 15. 提供模式
(provide 'weather)


使用说明

1. 预设选项

系统提供三种预设选项：

选项a: 3天 WeatherAPI (C-c w a)

• API提供商: WeatherAPI.com

• 预报天数: 3天

• 需要API密钥

• 包含详细的天气信息

选项b: 5天 OpenWeather (C-c w b)

• API提供商: OpenWeather

• 预报天数: 5天

• 需要API密钥

• 包含丰富的天气数据

选项c: 5天 Open-Meteo (默认) (C-c w c)

• API提供商: Open-Meteo

• 预报天数: 5天

• 无需API密钥

• 免费且无限制

2. 快速开始

显示选项菜单 (C-c w o)

M-x weather-show-options-menu


显示选项菜单，可选择预设选项或自定义设置。

快速显示天气 (C-c w w)

M-x weather-quick-show


使用当前设置显示所有城市的天气预报。

3. API密钥设置

WeatherAPI密钥

M-x weather-set-weatherapi-key


输入从 https://www.weatherapi.com(https://www.weatherapi.com) 获取的API密钥。

OpenWeather密钥

M-x weather-set-openweather-key


输入从 https://openweathermap.org/api(https://openweathermap.org/api) 获取的API密钥。

4. 选项菜单使用

在选项菜单中可以使用以下快捷键：

• 1 - 选择选项a: 3天 WeatherAPI

• 2 - 选择选项b: 5天 OpenWeather

• 3 - 选择选项c: 5天 Open-Meteo

• p - 手动选择API提供商

• d - 手动选择预报天数

• u - 切换温度单位

• i - 切换图标显示

• s - 显示天气

• q - 退出菜单

5. 异步获取特性

• 非阻塞界面：获取数据时Emacs不会卡住

• 进度显示：显示已获取城市数量

• 自动完成：所有数据获取完成后自动显示

• 错误处理：单个城市失败不影响其他城市

6. 显示格式

表格格式：

| 城市 | 日期       | 天气 | 最高温 | 最低温 | 降水量 | 降雨概率 |
|------|------------|------|--------|--------|---------|----------|
| 北京 | 2024-01-15 | ☀️  | 5.2°C  | -3.1°C | 0.00 mm | 10%      |
| 北京 | 2024-01-16 | 🌧️  | 3.8°C  | -4.5°C | 2.50 mm | 45%      |


降雨概率着色：

• 降雨概率 < 30%：正常显示

• 降雨概率 ≥ 30%：红色加粗显示

7. API特性对比

特性	Open-Meteo	OpenWeather	WeatherAPI
免费	✅ 是	⚠️ 有限制	⚠️ 有限制
API密钥	❌ 不需要	✅ 需要	✅ 需要
预报天数	✅ 3或5天	⚠️ 只能5天	✅ 3或5天
天气图标	✅ 支持	✅ 支持	✅ 支持
异步获取	✅ 支持	✅ 支持	✅ 支持
数据更新	实时	每3小时	实时

8. 故障排除

API密钥问题

;; 检查当前API提供商
(message "当前API提供商: %s" weather-api-provider)

;; 检查密钥设置
(if (eq weather-api-provider 'openweather)
    (message "OpenWeather密钥: %s" (if (string-empty-p openweather-api-key) "未设置" "已设置"))
  (if (eq weather-api-provider 'weatherapi)
      (message "WeatherAPI密钥: %s" (if (string-empty-p weatherapi-api-key) "未设置" "已设置"))
    (message "Open-Meteo无需密钥")))


网络连接问题

;; 检查API可用性
M-x weather-check-api-availability


异步获取失败

;; 查看消息缓冲区
*Messages* 缓冲区
;; 或
C-h e


9. 自定义配置

修改城市列表

(setq weather-cities
      '(("纽约"     "40.7128"   "-74.0060")
        ("伦敦"     "51.5074"   "-0.1278")
        ("东京"     "35.6762"   "139.6503")
        ("巴黎"     "48.8566"   "2.3522")
        ("悉尼"     "-33.8688"  "151.2093")))


永久设置选项

;; 在配置文件中设置默认选项
(setq weather-api-provider 'open-meteo)  ; 默认使用Open-Meteo
(setq weather-forecast-days 5)           ; 默认5天预报
(setq weather-units "metric")            ; 默认摄氏度
(setq weather-show-icons t)              ; 默认显示图标
(setq openweather-api-key "your-key")    ; 设置OpenWeather密钥
(setq weatherapi-api-key "your-key")     ; 设置WeatherAPI密钥


这个天气系统提供了三种完整的选项，支持异步获取，保持UTF-8编码，当降雨概率≥30%时用红色字体显示，并以Org表格格式输出，非常适合在Emacs中查看多城市天气预报。
