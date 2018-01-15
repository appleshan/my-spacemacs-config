;;; packages.el --- appleshan-org Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq appleshan-org-packages
    '(
      ;; deft
      (epa-file :location built-in)
      (org :location built-in)
      (org-agenda :location built-in)
      (org-archive :location built-in)
      (org-capture :location built-in)
      (org-clock :location built-in)
      (org-crypt :location built-in)
      (org-faces :location built-in)
      (org-list :location built-in)
      org-bullets
      ; org-password-manager
      (org-src :location built-in)
      (ob-core :location built-in)
      (ob-ditaa :location built-in)
      (ob-plantuml :location built-in)
      ; (ob-ledger :location built-in) ; 必须 init，才能使用
      org-sticky-header
      org-super-agenda
      ;; secretaria
      ;; unicode-fonts
      ))

;; List of packages to exclude.
(setq appleshan-org-excluded-packages '())

(defun appleshan-org/post-init-deft ()
  (progn
    (setq deft-use-filename-as-title t)
    (setq deft-use-filter-string-for-filename t)
    (spacemacs/set-leader-keys-for-major-mode 'deft-mode "q" 'quit-window)
    (setq deft-recursive t)
    (setq deft-extension "org")
    (setq deft-directory (concat user-projects-directory "org-notes/"))))

;; 借助 EasyPG package 原生支持GnuPG加密. 提供基本的GnuPG功能.
(defun appleshan-org/init-epa-file ()
  (use-package epa-file
    :defer t
    :config
    (progn
      ;; 总是使用对称加密
      (setq epa-file-encrypt-to nil)
      ;; 只會在目前的 session 記住這個密碼
      (setq epa-file-cache-passphrase-for-symmetric-encryption t)
      ;; 不允许自动保存
      (setq epa-file-inhibit-auto-save t)
      (setq epa-file-select-keys 0)
      ;; non-GUI password dialog. Test: (getenv "GPG_AGENT_INFO")
      (setenv "GPG_AGENT_INFO" nil)

      ;; Use org-mode for encrypted org file
      (add-to-list 'auto-mode-alist '("\\.org.gpg\\'" . org-mode)))))

(defun appleshan-org/post-init-org ()
  (add-hook 'org-mode-hook
    (lambda () (spacemacs/toggle-line-numbers-off)) 'append)

  (with-eval-after-load 'org
    (add-to-list 'auto-mode-alist
      '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

    (spacemacs|disable-company org-mode)

    ;; 防止不小心编辑了省略部分的内容
    (setq org-catch-invisible-edits 'smart)

    ;; 启用org-indent-mode
    (setq org-startup-indented t)

    ;; 不显示headline之间的空白行
    (setq org-cycle-separator-lines 0)

    ;; Add new easy templates
    (setq org-structure-template-alist
          (append '(("ex" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE")
                    ("sb" "#+BEGIN_SRC bash\n?\n#+END_SRC")
                    ("se" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
                    ("sp" "#+BEGIN_SRC python\n?\n#+END_SRC")
                    ("su" "#+BEGIN_SRC plantuml\n?\n#+END_SRC")
                    )
                org-structure-template-alist))

    ;;;;;;;;;;;;;;;;;;;;
    ;; 显示样式
    ;;;;;;;;;;;;;;;;;;;;

    ;;{{ 仅仅显示斜体字就好
    ;; @see https://github.com/lujun9972/emacs-document/blob/master/org-mode/%E5%B0%86org%E7%9C%8B%E6%88%90%E6%96%87%E5%AD%97%E5%A4%84%E7%90%86%E5%99%A8.org
    ;; /org italic/ 看起来就好像是斜体字被正则表达式的分隔符所包围了一样. 隐藏这些标记很简单
    (setq org-hide-emphasis-markers t)
    ;; 记住,这些斜杠字符(用于标示粗体的星号等其他字符也是一样)依然存在的,只是没有显示出来而已.
    ;; 想要修改这些标记也很简单,只要在之上按退格键就行.
    ;;}}

    ;;;;;;;;;;;;;;;;;;;;
    ;; TODO 状态触发器
    ;;;;;;;;;;;;;;;;;;;;

    ;; 当 TODO 状态发生更改时,自动添加/删除特定的 TAG ,这样方便 agenda view 中过滤任务:
    ;; org-todo-state-tags-triggers 的格式为:
    ;; `(state-change (tag . flag) …….)’
    ;; 这里 state-change 可以是一个表示 todo 状态的字符串,或者是符号 ’todo 或 ’done ,
    ;; 分别表示所有表示未完成任务的和以完成任务的 todo state
    (setq org-todo-state-tags-triggers
          (quote (("CANCELLED" ("CANCELLED" . t))
                  ("WAITING" ("WAITING" . t))
                  ("HOLD" ("WAITING") ("HOLD" . t))
                  (done ("WAITING") ("HOLD"))
                  ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
    ;; note:
    ;; * Moving a task to CANCELLED adds a CANCELLED tag
    ;; * Moving a task to WAITING adds a WAITING tag
    ;; * Moving a task to HOLD adds WAITING and HOLD tags
    ;; * Moving a task to a done state removes WAITING and HOLD tags
    ;; * Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
    ;; * Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
    ;; * Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags

    ;;;;;;;;;;;;;;;;;;;;
    ;; Logging
    ;;;;;;;;;;;;;;;;;;;;

    ;; 将 log 存入 drawer 中
    (setq org-log-into-drawer t)

    ;; 设置 log 存放在 task 的哪个位置
    (setq org-log-state-notes-insert-after-drawers nil)

    ;; todo keywords的定义也与log息息相关

    ;; other

    ;; 完成重复任务时重设所有子任务
    (setq org-default-properties
      (cons "RESET_SUBTASKS" org-default-properties))

    ;; 在导出时,不导出时间戳
    (setq org-export-with-timestamps nil)

    ;; 让正文中的 plain list 也具有折叠的能力
    (setq org-cycle-include-plain-lists t)

    ;; Create unique IDs for tasks when linking
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

    (setq org-tags-match-list-sublevels nil)

  ))

(defun appleshan-org/post-init-org-agenda()
  (with-eval-after-load 'org-agenda
    ;; Custom Key Bindings
    (spacemacs/set-leader-keys
      ;; refile task
      "or"  'org-agenda-refile)

    (setq org-agenda-inhibit-startup t)   ;; ~50x speedup
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    (setq org-agenda-window-setup 'current-window)

    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)

    (unless (file-exists-p org-gtd-dir)
      (make-directory org-gtd-dir))

    ;; 设置agenda的数据来源
    ;; org-agenda-files中的元素还可以是目录，这时目录下的所有匹配
    ;; `org-agenda-file-regexp’的文件都自动加入 agenda .
    (setq org-agenda-files
      (list
          (concat org-gtd-dir "inbox.org")
          (concat org-gtd-dir "project.org")
          (concat org-gtd-dir "task.org")
          (concat org-gtd-dir "finished.org")
          (concat org-gtd-dir "trash.org")
          (concat org-gtd-dir "memorial-day.org")
          ))

    ;;;;;;;;;;;;;;;;;;;;
    ;; 配置 agenda view
    ;;;;;;;;;;;;;;;;;;;;

    ;; Can be day, week, fortnight, month, year, or any number of days.
    (setq org-agenda-span 'week)

    ;; Do not dim blocked tasks
    ;(setq org-agenda-dim-blocked-tasks nil)

    ;; Compact the block agenda view
    ;(setq org-agenda-compact-blocks t)

    ;; Custom agenda command definitions
    ;; An entry without a cookie is treated just like priority ' B '.
    ;; So when create new task, they are default 重要且紧急
    (setq org-agenda-custom-commands
          '(("c" "Super Agenda" agenda
             (org-super-agenda-mode)
             ((org-super-agenda-groups
               '(;; Each group has an implicit boolean OR operator between its selectors.
                 (:name "Today"  ; Optionally specify section name
                        :time-grid t  ; Items that appear on the time grid
                        :todo "TODAY")  ; Items that have this TODO keyword
                 (:name "Important"
                        ;; Single arguments given alone
                        :tag "bills"
                        :priority "A")
                 ;; Set order of multiple groups at once
                 (:order-multi (2 (:name "Shopping in town"
                                         ;; Boolean AND group matches items that match all subgroups
                                         :and (:tag "shopping" :tag "@town"))
                                  (:name "Food-related"
                                         ;; Multiple args given in list with implicit OR
                                         :tag ("food" "dinner"))
                                  (:name "Personal"
                                         :habit t
                                         :tag "personal")
                                  (:name "Space-related (non-moon-or-planet-related)"
                                         ;; Regexps match case-insensitively on the entire entry
                                         :and (:regexp ("space" "NASA")
                                                       ;; Boolean NOT also has implicit OR between selectors
                                                       :not (:regexp "moon" :tag "planet")))))
                 ;; Groups supply their own section names when none are given
                 (:todo "WAITING" :order 8)  ; Set order of this section
                 (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                        ;; Show this group at the end of the agenda (since it has the
                        ;; highest number). If you specified this group last, items
                        ;; with these todo keywords that e.g. have priority A would be
                        ;; displayed in that group instead, because items are grouped
                        ;; out in the order the groups are listed.
                        :order 9)
                 (:priority<= "B"
                              ;; Show this section after "Today" and "Important", because
                              ;; their order is unspecified, defaulting to 0. Sections
                              ;; are displayed lowest-number-first.
                              :order 1)
                 ;; After the last group, the agenda will display items that didn't
                 ;; match any of these groups, with the default order position of 99
                 )
               ))
             (org-agenda nil "a"))))
  ))

;;;;;;;;;;;;;;;;;;;;
;; 归档
;;;;;;;;;;;;;;;;;;;;

(defun appleshan-org/init-org-archive ()
  (with-eval-after-load 'org-archive
    ; (require 'org-archive)

    ;; 归档时保持TODO state不变
    (setq org-archive-mark-done nil)

    ;; 通过设置`org-archive-mark-done’可以指定归档的位置
    (setq org-archive-location "%s_archive::* Archived Tasks")
    ;; 带有`Archive’ tag的entry,默认情况下不会被展开,但可以使用`C-TAB’强制展开
  ))

;;;;;;;;;;;;;;;;;;;;
;; 配置 org-capture
;;;;;;;;;;;;;;;;;;;;

(defun appleshan-org/init-org-capture ()
  (with-eval-after-load 'org-capture
    ; (require 'org-capture)

    ;; Capure模板
    ;; 所有 caputre 的 task 都先暂存入 inbox.org 中，再 refile 到各个 org 文件中
    ;; 我们将 task 划分为一下几类:
    ;
    ; A phone call(p)
    ; A meeting (m)
    ; An email I need to respond to (r)
    ; A new task (t)
    ; A new note (n)
    ; An interruption (j)
    ; A new habit (h)
    ;
    ;; Capture templates for: TODO tasks, phone calls, meetings

    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal

    (setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-gtd-dir "inbox.org") "Tasks")
             "** TODO %? %^G\n  Created: %U \n  %i")
        ("T" "Scheduled Todo" entry (file+headline (concat org-gtd-dir "inbox.org") "Tasks")
             "** TODO %? %^G\n SCHEDULED: %^{ Sheduled: }T Created: %U \n  %i")
        ("m" "Meeting" entry (file+headline (concat org-gtd-dir "inbox.org") "Tasks")
             "* MEETING with %? :Meeting:\n%U" :clock-in t :clock-resume t)
        ("p" "Phone call" entry (file+headline (concat org-gtd-dir "inbox.org") "Tasks")
             "* PHONE %? :Phone:\n%U" :clock-in t :clock-resume t)
        ))

    ;; Set default column view headings: Task Effort Clock_Summary
    (setq org-columns-default-format
          ; "%50ITEM(Task) %10TODO %3PRIORITY %TAGS %10Effort(Effort){:} %10CLOCKSUM"
          "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM{Total}")
    ;; global Effort estimate values
    ;; global STYLE property values for completion
    (setq org-global-properties (quote (
      ("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
      ("STYLE_ALL" . "habit"))))

    ;; Tags with fast selection keys
    (setq org-tag-alist (quote ((:startgroup)
                                ("Office" . ?O)
                                ("Home" . ?H)
                                (:endgroup)
                                ("Computer" . ?c)
                                ("Reading" . ?r)
                                ("Project" . ?p))))
))

;;;;;;;;;;;;;;;;;;;;
;; Time Clocking
;;;;;;;;;;;;;;;;;;;;

(defun appleshan-org/init-org-clock ()
  (;progn
    with-eval-after-load 'org-clock
    ;; Clock setup
    ; (require 'org-clock)
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)

    ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
    (setq org-clock-history-length 23)
    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)
    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Separate drawers for clocking and logs
    (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
    ;; Save clock data and state changes and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks 
    ;; with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)
    ;; Clock out when moving task to a done state
    (setq org-clock-out-when-done t)
    ;; Save the running clock and all clock history when exiting Emacs, load it 
    ;; on startup
    (setq org-clock-persist t)
    ;; Do not prompt to resume an active clock
    (setq org-clock-persist-query-resume nil)
    ;; Enable auto clock resolution for finding open clocks
    (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
    ;; Include current clocking task in clock reports
    (setq org-clock-report-include-clocking-task t)
    ;; Resolve open clocks if the user is idle for more than 10 minutes.
    (setq org-clock-idle-time 10)

    ;; 设置default clock in task

    ;; 使用clock history来clock in先前的tasks

    ;; 修改clock记录的时间戳
    ;; 在时间戳上用S-<up>可以增加时间戳的值, S-<down>可以减少时间戳的值.
    ;; 下面的配置说明当使用S-<up>/S-<down>修改时间戳时，以１分钟为单位来修改
    (setq org-time-stamp-rounding-minutes '(1 1))

    ;; 设置mode-line

    ;; 当总计的时间超过了预估的时间时,替换mode-line背景色为红色,以示提醒
    (custom-set-faces
      ;; custom-set-faces was added by Custom.
      ;; If you edit it by hand, you could mess it up, so be careful.
      ;; Your init file should contain only one such instance.
      ;; If there is more than one, they won't work right.
     '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))

    ;; 通过设置`:clock-in t’使得在captre task时自动开始clock in.
    ;; 设置`:clock-resume t’则使得capture task完成后,自动恢复原task的clock in.
    ;; 但这就会产生一个问题,若capture task的时间小于1分钟,则可能有大量的计时为
    ;;  0:00 的记录存在,
    ;; 这些记录需要清理

    ;; Remove empty LOGBOOK drawers on clock out
    (defun bh/remove-empty-drawer-on-clock-out ()
      (interactive)
      (save-excursion
        (beginning-of-line 0)
        ;; Following line from original document by Bernt Hansen
        ;; will lead to an error, next to it is the corrected form.
        ;; (org-remove-empty-drawer-at "LOGBOOK" (point))
        (org-remove-empty-drawer-at (point))))

    (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

    ;;;;;;;;;;;;;;;;;;;;
    ;; 评估任务的工作量
    ;;;;;;;;;;;;;;;;;;;;

    ;; 通过为 task 增加 `Effort’ 属性，可以为任务设置一个评估的工作量，若
    ;; clock tracking 的时间超过了这个评估的工作量,则会提出警告:
    ;;
    ; * NEXT Document my use of org-mode
    ;   :PROPERTIES:
    ;   :CLOCK_MODELINE_TOTAL: today
    ;   :Effort:   1:00
    ;   :END:
    ;;
    ;; 可以设置clock tracking的时间到达预估工作量时的提醒声音
    (setq org-clock-sound t)
    ))

;;;;;;;;;;;;;;;;;;;;
;; org-crypt 加密
;;;;;;;;;;;;;;;;;;;;

;; http://coldnew.github.io/blog/2013/07/13_5b094.html
;; 使用`org-crypt’库,可以自动将带”:secret:” tag 的 headline ,在写入时加密存储.
;; 该功能对于想要将密码等隐私消息存入org文件带来便利.

(defun appleshan-org/init-org-crypt ()
  (;progn
    with-eval-after-load 'org-crypt
    ; (require 'org-crypt)

    ;; 保存前,自動加密回去
    (org-crypt-use-before-save-magic)

    ;; 設定要加密的 tag 標籤為 secret
    (setq org-crypt-tag-matcher "secret")

    ;; 设置 secret 标签不参与继承,避免造成重複加密
    ;; (但是子項目還是會被加密喔)
    (setq org-tags-exclude-from-inheritance (quote ("secret")))

    ;; GPG key to use for encryption
    ;; Either the Key ID or set to nil to use symmetric encryption.
    (setq org-crypt-key nil)

    ;; 要想解密 headline,则需要在光标定位到加密内容处,然后执行`M-x org-decrypt-entry’
    ;; 默认情况下, Emacs 会定时自动保持在编辑的文件,
    ;; 若此时在编辑的文件为密码文件且内容已经被解密,则可能存在将解密后的文本保存
    ;; 到磁盘上, 从而造成敏感信息泄露的情况,因此一般我们在编辑 crypt 文件时,取消
    ;; 自动保存功能
    (setq org-crypt-disable-auto-save t)
))

;;;;;;;;;;;;;;;;;;;;
;; org-faces
;;;;;;;;;;;;;;;;;;;;

(defun appleshan-org/init-org-faces ()
  (with-eval-after-load 'org-faces
    ; (require 'org-faces)
    (setq org-todo-keyword-faces
          (quote (("TODO" :foreground "red" :weight bold)
                  ("STARTED" :foreground "blue" :weight bold)
                  ("WAITING" :foreground "orange" :weight bold)
                  ("APPT" :foreground "magenta" :weight bold)
                  ("DONE" :foreground "forest green" :weight bold)
                  ("CANCELLED" :foreground "forest green" :weight bold)
                  ("DEFERRED" :foreground "forest green" :weight bold)
                  ("MEETING" :foreground "forest green" :weight bold)
                  ("PHONE" :foreground "forest green" :weight bold))))

;; Priority
(setq org-priority-faces
      '((?A . (:foreground "white" :background "dark red"
                           :box '(:color "red" :line-width 3 :style released-button)))
        (?B . (:foreground "white" :background "dark slate blue"
                           :box '(:color "white" :line-width 3 :style released-button)))
        (?C . (:foreground "white" :background "dim gray"
                           :box '(:color "dim gray" :line-width 3 :style released-button)))
        ))
;; (set-face-attribute 'org-priority nil
;;                     :box '(:color "red" :line-width 3 :style released-button)
;;                     :bold nil)

;; inline code face => src_ruby{require 'something'}
;;
;; (REGEXP . FACE)
;;     Highlight REGEXP with FACE
;; (REGEXP N FACE)
;;     Highlight group N in REGEXP with FACE
;; (REGEXP (N1 FACE1) (N2 FACE2) (N3 FACE3) …)
;;     Highlight group Ni in REGEXP with FACEi
;;
;; src_lang{code...}[:header arguments] / NOTE: override by `org-verbatim'.
;; result in following =[result]=
(setq org-babel-exp-inline-code-template "src_%lang[%switches%flags]{%body}"
      org-babel-inline-result-wrap "=> (~%s~)" ; or "=%s=", "~%s~"
      )

))

(defun appleshan-org/init-org-list ()
  (progn
    (require 'org-list)

    ;; 允许使用字母作为list bullet
    (setq org-list-allow-alphabetical t)

    ;; 自动切换list bullet
    ;; 若每个层级的list都使用同样的list bullet,则可能造成难于区分哪个list entry
    ;; 是属于哪个层级的. org-mode提供了当改变list层级时自动改变list bullet的机制
    (setq org-list-demote-modify-bullet '(("+" . "-")
                                          ("*" . "-")
                                          ("1." . "-")
                                          ("1)" . "-")
                                          ("A)" . "-")
                                          ("B)" . "-")
                                          ("a)" . "-")
                                          ("b)" . "-")
                                          ("A." . "-")
                                          ("B." . "-")
                                          ("a." . "-")
                                          ("b." . "-")))
    ))

;; 修改 org 文件中各层级的 headline 前显示的标志
(defun appleshan-org/post-init-org-bullets ()
  (with-eval-after-load 'org-bullets
    (setq org-bullets-bullet-list
          '("✡" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇"))
    ))

(defun appleshan-org/init-org-password-manager ()
  (use-package org-password-manager
    :defer t
    :config
    (progn
      (add-hook 'org-mode-hook 'org-password-manager-key-bindings))))

(defun appleshan-org/init-org-src ()
  (;progn
   with-eval-after-load 'org-src
    ; (require 'org-src)
    ;; Use puml mode when editing plantuml blocks with C-c '
    (add-to-list 'org-src-lang-modes (quote ("plantuml" . puml)))

    ;; http://wenshanren.org/?p=327
    (defun appleshan-org/org-insert-src-block (src-code-type)
      "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
      (interactive
        (let ((src-code-types
               '("emacs-lisp" "python" "bash" "java" "js" "css" "calc" "plantuml"
                 "sql" "ditaa" "lisp" "org" "scheme" "sqlite")))
             (list (ido-completing-read "Source code type: " src-code-types))))
      (progn
        (newline-and-indent)
        (insert (format "#+BEGIN_SRC %s\n" src-code-type))
        (newline-and-indent)
        (insert "#+END_SRC\n")
        (previous-line 2)
        (org-edit-src-code)))

    (add-hook 'org-mode-hook
      '(lambda ()
      ;; keybinding for editing source code blocks
      (local-set-key (kbd "C-c s e") 'org-edit-src-code)
      ;; keybinding for inserting code blocks
      (local-set-key (kbd "C-c i s") 'appleshan-org/org-insert-src-block)))
    ))

;; TODO: very slow!!!!
(defun appleshan-org/init-ob-core ()
  (with-eval-after-load 'org
    ; (require 'ob-core)
    ;;;;;;;;;;;;;;;;;;;;
    ;; org-babel配置
    ;;;;;;;;;;;;;;;;;;;;

    ;; 设置可以load的代码块
    (org-babel-do-load-languages
      'org-babel-load-languages
      '((ditaa . t)
        (emacs-lisp . t)
        (ledger . t)
        (org . t)
        (python . t)
        (sh . t)
        (sql . nil)
        (plantuml . t)))

    (defun org/display-inline-images ()
      (condition-case nil
          (org-display-inline-images)
        (error nil)))

    (add-hook 'org-babel-after-execute-hook 'org/display-inline-images 'append)

    ;; Make babel results blocks lowercase
    (setq org-babel-results-keyword "results")

    ;; C-c C-c执行代码块时,不需要确认
    ;; Do not prompt to confirm evaluation
    ;; This may be dangerous - make sure you understand the consequences
    ;; of setting this -- see the docstring for details
    (setq org-confirm-babel-evaluate nil)
))

;; ditaa 工具能够帮我们把 ASCII 图转成漂亮的架构图片
;; @see http://ditaa.sourceforge.net/
(defun appleshan-org/init-ob-ditaa ()
  (use-package ob-ditaa
    :config
    ;; (setq org-ditaa-jar-path "/opt/java/ditaa0_9/ditaa0_9.jar")
    (setq org-ditaa-jar-path
      (directory-files (concat (getenv "JAR_PATH") "/ditaa0_9") t "ditaa[[:ascii:]]+\\.jar$"))
    ))

;; plantuml 工具能够帮我们把 UML 描述代码 转成漂亮的 UML 图片
;; @see http://plantuml.com/
(defun appleshan-org/init-ob-plantuml ()
  (use-package ob-plantuml
    :config
    (setq org-plantuml-jar-path (concat (getenv "JAR_PATH") "/plantuml.jar"))
    ))

(defun appleshan-org/init-ob-ledger ()
  (use-package ob-ledger))

(defun appleshan-org/init-org-sticky-header ()
  (use-package org-sticky-header
    :defer t
    :init (require 'org-sticky-header)
    :config (add-hook 'org-mode-hook 'org-sticky-header-mode)))

(defun appleshan-org/init-org-super-agenda ()
  (use-package org-super-agenda
    :config (org-super-agenda-mode)))

(defun appleshan-org/init-secretaria ()
  (use-package secretaria
    :ensure nil
    :preface
    (use-package alert)
    (use-package f)
    (use-package s)
    :config
    ;; use this for getting a reminder every 30 minutes of those tasks scheduled
    ;; for today and which have no time of day defined.
    (add-hook 'after-init-hook
      #'secretaria-today-unknown-time-appt-always-remind-me)))

(defun appleshan-org/init-unicode-fonts ()
  (use-package unicode-fonts
    :init
    (progn
      (require 'unicode-fonts)
      (unicode-fonts-setup))
    ))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
