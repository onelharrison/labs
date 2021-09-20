#lang racket

(require racket/trace)

; variable "varname" {
;   type = string
;   default = "something"
; }
;
; variable "varname2" {
;   type = list(string)
;   default = [var.varname, "something else"]
; }

; tfvar

(define (make-tfvar name value)
  (cons 'tfvar (cons name value)))

(define (tfvar-name tfvar)
  (cadr tfvar))

(define (tfvar-value tfvar)
  (cddr tfvar))

; defns (e.g. provider, resource, and data definitions)

; defn
; - type (quote)
; - name (quote)
; - value (list pair)

(define (make-defn type name value)
  (cons 'defn (cons type (cons name value))))

(define (defn-type defn)
  (cadr defn))

(define (defn-name defn)
  (caddr defn))

(define (defn-value defn)
  (cdddr defn))

(define (lookup-defn defns defn) ; TODO: simplify
  (cond ((empty? defns) #f)
	 (else (or (and (eq? (defn-type (car defns)) (defn-type defn))
			(eq? (defn-name (car defns)) (defn-name defn))
			(car defns))
		   (lookup-defn (cdr defns) defn)))))

(define (defn-value-equal? defn-value:alpha defn-value:beta)
  (let* ((defn-value-keys (append (map car defn-value:alpha) (map car defn-value:beta))) ; TODO: unique
	 (diff (filter (lambda (defn-value-key)
			 (not (equal? (assoc defn-value-key defn-value:alpha)
                                      (assoc defn-value-key defn-value:beta))))
		       defn-value-keys)))
    (and (eq? (length defn-value:alpha) (length defn-value:beta))
         (empty? diff))))

(define (defn-equal? defn:alpha defn:beta)
  (and (eq? (defn-type defn:alpha) (defn-type defn:alpha))
       (eq? (defn-name defn:alpha) (defn-name defn:beta))
       (defn-value-equal? (defn-value defn:alpha) (defn-value defn:beta))))

; tfstate

(define (make-tfstate defns)
  (cons 'tfstate defns))

(define (tfstate-defns tfstate)
  (cdr tfstate))

(define empty-tfstate
  (make-tfstate '()))

(define (destroy-tfstate tfstate defns)
  ; keep defns that aren't in `defns`
  (make-tfstate (filter (lambda (defn) (not (lookup-defn defns defn))) (tfstate-defns tfstate))))

(define (add-tfstate tfstate defns)
  ; doesn't check for type-name uniqueness, but should
  (make-tfstate (append (tfstate-defns tfstate) defns)))

(define (modify-tfstate tfstate defns)
  (add-tfstate (destroy-tfstate tfstate defns) defns))

; operations
;
; (define (tf-init)
;   empty-tfstate)
;
; (define (tf-plan tfstate tfvars locals defns)
;   '())
;
; (define (tf-apply tfstate tfvars locals defns)
;   '())
;
; (define (tf-destroy tfstate tfvars locals defns)
;   '())

; demos

; demo - plan with tfvars as let
; (let ((tfvars (list (make-tfvar "image_id" "vm-win-serv2012-8a7fd"))))
;   (tf-plan (tf-init) tfvars '() '()))
;
; ; demo - plan with tfvars as lambda
; ((lambda (tfvars)
;      (tf-apply (tf-init) tfvars '() '()))
;    (list (make-tfvar "image_id" "vm-win-serv2012-8a7fd")))
;
; ; demo - plan with tfvars and local as let and let*
; (let ((tfvars (list (make-tfvar "image_id" "vm-win-serv2012-8a7fd"))))
;   (let* ((hello 'hello)
; 	 (hello-there (cons hello 'there))
; 	 (locals (list hello hello-there)))
;      (tf-apply empty-tfstate tfvars locals '())))
;
; ; demo - plan with tfvars and local as lambda and let*
; ((lambda (tfvars)
;    (let* ((hello 'hello)
; 	  (hello-there (cons hello 'there))
; 	  (locals (list hello hello-there)))
;      (tf-apply empty-tfstate tfvars locals '())))
;    (list (make-tfvar "image_id" "vm-win-serv2012-8a7fd")))
;
; ; demo - plan with tfvars and local as lambda and local
; ((lambda (tfvars)
;    (local [(define hello 'hello)
; 	   (define hello-there (cons hello 'there))
; 	   (define locals (list hello hello-there))]
;      (tf-apply empty-tfstate tfvars locals '())))
;    (list (make-tfvar "image_id" "vm-win-serv2012-8a7fd")))


; tfmodule

(define empty-tfmodule
  (cons 'tfmodule '()))

(define (tfmodule-defns tfmodule)
  (cdr tfmodule))

; tfmodule operations

(define (terraform:init)
  (make-tfstate '()))

(define (terraform:plan/diff-add tfstate tfmodule)
  ; defns in tfmodule and not in tfstate
  (let ((tfmodule-defns* (tfmodule-defns tfmodule))
	(tfstate-defns* (tfstate-defns tfstate)))
    (filter (lambda (defn) (not (lookup-defn tfstate-defns* defn))) tfmodule-defns*)))

(define (terraform:plan/diff-modify tfstate tfmodule)
  ; defns in tfmodule and tfstate but values are different
  (let ((tfstate-defns* (tfstate-defns tfstate)))
    (filter (lambda (defn)
	      (let ((defn* (lookup-defn tfstate-defns* defn)))
		(begin
		  (and defn* (not (defn-value-equal? (defn-value defn*) (defn-value defn)))))))
	    (tfmodule-defns tfmodule))))

(define (terraform:plan/diff-destroy tfstate tfmodule)
  ; defns in tfstate and not in tfmodule
  (let ((tfmodule-defns* (tfmodule-defns tfmodule))
	(tfstate-defns* (tfstate-defns tfstate)))
    (filter (lambda (defn) (not (lookup-defn tfmodule-defns* defn))) tfstate-defns*)))

(define (terraform:plan tfstate tfmodule)
  ; returns a diff of addtions, modifications, and destroys to be made
  (let ((adds (terraform:plan/diff-add tfstate tfmodule))
	(mods (terraform:plan/diff-modify tfstate tfmodule))
	(destroys (terraform:plan/diff-destroy tfstate tfmodule)))
    (list (cons 'add adds) (cons 'modify mods) (cons 'destroy destroys))))

(define (terraform:apply tfstate tfplan)
  ; extend tfstate with adds
  (let* ((tfstate:alpha (destroy-tfstate tfstate (cdr (assoc 'destroy tfplan))))
	 (tfstate:beta  (modify-tfstate tfstate:alpha (cdr (assoc 'modify tfplan))))
	 (tfstate:gamma (add-tfstate tfstate:beta (cdr (assoc 'add tfplan)))))
    tfstate:gamma))

(define (terraform:destroy tfstate (tfplan #f))
  (if tfplan
    (terraform:apply tfstate tfplan)
    (terraform:apply tfstate (list
       		               (cons 'add '())
       		               (cons 'modify '())
       		               (cons 'destroy (tfstate-defns tfstate))))))

; terraform equivalent - defining a module
;
; variable "image_id" {
;     type = string
; }
;
; resource "vm" "vm-win-serv" {
;     image_id = var.image_id
; }
;
; resource "vm" "vm-win-serv2" {
;     image_id = var.image_id
; }
(define (tfmodule:vm tfvar:image-id)
   (local [(define locals '())
	   (define defn:resource:vm (make-defn 'resource
					       'vm-win-serv
					       (list (cons 'image-id (tfvar-value tfvar:image-id)))))
           (define defn:resource:vm2 (make-defn 'resource
					       'vm-win-serv2
					       (list (cons 'image-id (tfvar-value tfvar:image-id)))))
	   (define defns (list defn:resource:vm defn:resource:vm2))]
     (cons 'tfmodule defns)))

(define (tfmodule:vm3 tfvar:image-id)
   (local [(define locals '())
	   (define defn:resource:vm (make-defn 'resource
					       'vm-win-serv
					       (list (cons 'image-id (tfvar-value tfvar:image-id)))))
           (define defn:resource:vm2 (make-defn 'resource
					       'vm-win-serv2
					       (list (cons 'image-id (tfvar-value tfvar:image-id)))))
	   (define defn:resource:vm3 (make-defn 'resource
					       'vm-win-serv3
					       (list (cons 'image-id (tfvar-value tfvar:image-id)))))

	   (define defns (list defn:resource:vm defn:resource:vm2 defn:resource:vm3))]
     (cons 'tfmodule defns)))

; terraform equivalent - using a module
;
; module "vm_win_serv" "modules/vm" {
;     image_id = "vm-win-serv2012-8a7fd"
; }
(define vm-win-serv
  (tfmodule:vm
    (make-tfvar "image-id" "vm-win-serv2012-8a7fd")))

(define vm-win-serv3
  (tfmodule:vm3
    (make-tfvar "image-id" "vm-win-serv2012-8a7fx")))


; terraform init
(define tfstate:alpha (terraform:init))

; terraform plan - bring up
(define tfplan:up (terraform:plan tfstate:alpha vm-win-serv))

; (displayln tfplan:up)

; terraform apply
(define tfstate:beta (terraform:apply tfstate:alpha tfplan:up))
(displayln tfstate:beta)

; terraform plan - bring down
; (define tfplan:down (terraform:plan tfstate:beta empty-tfmodule))

(terraform:plan tfstate:beta vm-win-serv3)
; (add-tfstate (destroy-tfstate tfstate:beta (tfmodule-defns vm-win-serv3)) (tfmodule-defns vm-win-serv3))
; (terraform:plan tfstate:beta vm-win-serv3)

; terraform destroy
; (terraform:destroy tfstate:beta)
