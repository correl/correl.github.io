
;;; git-graph.el --- Generate git-style graphs using graphviz

;; Copyright (c) 2015 Correl Roush <correl@gmail.com>

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'dash)

(defun git-graph/make-node (id &optional parents options)
  (list id parents options))

(defun git-graph/node-id (node)
  (nth 0 node))

(defun git-graph/node-parents (node)
  (nth 1 node))

(defun git-graph/node-group (node)
  (cdr (assoc 'group (nth 2 node))))

(defun git-graph/node-label (node)
  (cdr (assoc 'label (nth 2 node))))

(defun git-graph/+ (a b)
  (append a
          (-remove (lambda (node)
                     (assoc (git-graph/node-id node) a))
                   b)))

(defun git-graph/to-graphviz (id nodes)
  (string-join
   (list
    (concat "digraph " id " {")
    "bgcolor=\"transparent\";"
    "rankdir=\"LR\";"
    "node[width=0.15,height=0.15,shape=point,fontsize=8.0];"
    "edge[weight=2,arrowhead=none];"
    (string-join
     (-map #'git-graph/to-graphviz-node nodes)
     "\n")
     (string-join
      (-uniq (-flatten (-map
                        (lambda (node) (git-graph/to-graphviz-edges node nodes))
                        nodes)))
      "\n")
      "}")
   "\n"))
(defun git-graph/to-graphviz-pretty (id nodes)
  (with-temp-buffer
    (graphviz-dot-mode)
    (insert (git-graph/to-graphviz id nodes))
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defun git-graph/to-graphviz-node-id (id)
  (format "\"%s\"" id))
(defun git-graph/to-graphviz-node (node)
  (let ((node-id (git-graph/to-graphviz-node-id (git-graph/node-id node))))
    (concat node-id
            (git-graph/to-graphviz-node--attributes node)
            ";")))

(defun git-graph/to-graphviz-node--attributes (node)
  (let ((attributes (git-graph/to-graphviz-node--compute-attributes node)))
    (and attributes
         (concat "["
                 (mapconcat (lambda (pair)
                              (format "%s=\"%s\""
                                      (car pair) (cdr pair)))
                            attributes
                            ", ")
                 "]"))))

(defun git-graph/to-graphviz-node--compute-attributes (node)
  (-filter #'identity
           (append (and (git-graph/node-group node)
                        (list (cons 'group (git-graph/node-group node))))
                   (and (git-graph/node-label node)
                        (list (cons 'shape 'box)
                              (cons 'label (git-graph/node-label node)))))))

(defun git-graph/to-graphviz-edges (node &optional nodelist)
  (let ((node-id (git-graph/node-id node))
        (parents (git-graph/node-parents node))
        (node-ids (-map #'git-graph/node-id nodelist)))
    (-map (lambda (parent)
            (unless (and nodelist (not (member parent node-ids)))
              (git-graph/to-graphviz-edge node-id parent)))
          parents)))

(defun git-graph/to-graphviz-edge (from to)
  (concat
   (git-graph/to-graphviz-node-id to)
   " -> "
   (git-graph/to-graphviz-node-id from)
   ";"))

(defun git-graph/group-topo (nodelist)
  (reverse
   (car
    (-reduce-from
     (lambda (acc node)
       (let* ((grouped-nodes (car acc))
              (group-stack (cdr acc))
              (node-id (git-graph/node-id node))
              (group-from-stack (--if-let (assoc node-id group-stack)
                                    (cdr it)))
              (group (or group-from-stack node-id))
              (parents (git-graph/node-parents node))
              (first-parent (first parents)))
         (if group-from-stack
             (pop group-stack))
         (if (and first-parent (not (assoc first-parent group-stack)))
             (push (cons first-parent group) group-stack))
         (cons (cons (git-graph/make-node node-id
                                    parents
                                    `((group . ,group)
                                      (label . ,(git-graph/node-label node))))
                     grouped-nodes)
               group-stack)))
     nil
     nodelist))))

(defun git-graph/git-execute (repo-url command &rest args)
  (with-temp-buffer
    (shell-command (format "git -C \"%s\" %s"
                           repo-url
                           (string-join (cons command args)
                                        " "))
                   t)
    (buffer-string)))
(defun git-graph/git-rev-list (repo-url head)
  (-map (lambda (line) (split-string line))
        (split-string (git-graph/git-execute
                       repo-url
                       "rev-list" "--topo-order" "--parents" head)
                      "\n" t)))
(defun git-graph/git-label (repo-url rev)
  (let ((name (string-trim
               (git-graph/git-execute repo-url
                                      "name-rev" "--name-only" rev))))
    (unless (s-contains? "~" name)
      name)))
(defun git-graph/git-graph-head (repo-url head)
  (git-graph/group-topo
   (-map (lambda (rev-with-parents)
           (let* ((rev (car rev-with-parents))
                  (parents (cdr rev-with-parents))
                  (label (git-graph/git-label repo-url rev)))
             (git-graph/make-node rev parents
                                  `((label . ,label)))))
         (git-graph/git-rev-list repo-url head))))
(defun git-graph/git-load (repo-url heads)
  (-reduce #'git-graph/+
           (-map (lambda (head)
                   (git-graph/git-graph-head repo-url head))
                 heads)))

(provide 'git-graph)
;;; git-graph.el ends here
