;---------------------------------------------------;
; IA
; Entrega 2	
; Agente jogador de Tetris
;---------------------------------------------------;
; Grupo 34
;
; 79664 - Joao Serras
; 79714 - Daniel Caramujo
; 79719 - Francisco Santos
;---------------------------------------------------;
; IST LEIC-A
; 30-11-2015
;---------------------------------------------------;


; constantes ---------------------------------------;
(defconstant pontuacao-max-peca-i 800)
(defconstant pontuacao-max-peca-j 500)
(defconstant pontuacao-max-peca-l 500)
(defconstant pontuacao-max-peca-s 300)
(defconstant pontuacao-max-peca-z 300)
(defconstant pontuacao-max-peca-t 300)
(defconstant pontuacao-max-peca-o 300)
(defconstant infinito most-positive-fixnum)
(defconstant coefH1 20)
(defconstant coefH2 10)
;---------------------------------------------------;


; estruturas ---------------------------------------;
; tipo estado
(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)
; tipo problema
(defstruct problema estado-inicial solucao accoes resultado custo-caminho)
; abstracoes
(defstruct capsula resultado valor-f)
(defstruct sucessor no-filho accao)
;---------------------------------------------------;

 
; codigo -------------------------------------------;


; funcoes 2.1.1 tipo accao -------------------------;
(defun cria-accao (int array)
	(cons int array))
	
	
(defun accao-coluna (a)
	(car a))
	
	
(defun accao-peca (a)
	(cdr a))
	

; funcoes 2.1.2 tipo tabuleiro ---------------------;


(defun cria-tabuleiro ()
	(list 
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)
		(make-list 10)))


(defun copia-tabuleiro (tabuleiro)
	(let ((res nil))
		(dolist (i tabuleiro res)
			(setf res (append res (list (copy-list i)))))))


(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
  (nth coluna (nth linha tabuleiro)))


(defun tabuleiro-altura-coluna (tabuleiro coluna)
	(labels 
		((tabuleiro-altura-coluna-aux (tabuleiro coluna)	
			(if (or (equal tabuleiro nil) (not (equal (nth coluna (first tabuleiro)) NIL)))
				0
				(+ 1 (tabuleiro-altura-coluna-aux (rest tabuleiro) coluna))))) 
		(- 18 (tabuleiro-altura-coluna-aux (reverse tabuleiro) coluna))))


(defun tabuleiro-linha-completa-p (tabuleiro linha)
	(let ((l (nth linha tabuleiro)))
	(dolist (i l T)
		(if (equal i NIL)
			(return NIL)))))
			
			
(defun tabuleiro-preenche! (tabuleiro linha coluna)
  (unless (or (> linha 17) (> coluna 9) (< linha 0) (< coluna 0))
    (setf (nth coluna (nth linha tabuleiro)) T)))
  
  
(defun tabuleiro-remove-linha! (tabuleiro linha)
	(replace tabuleiro  tabuleiro :start1 linha :end1 nil :start2 (+ linha 1) :end2 nil)
	(replace tabuleiro (list (make-list 10)) :start1 17 :end1 nil :start2 0 :end2 nil))
	

(defun tabuleiro-topo-preenchido-p (tabuleiro)
  (let ((res NIL))
  (dolist (i (nth 17 tabuleiro) res)
    (if (equal T i)
        (setf res T)))))


(defun tabuleiros-iguais-p (tabuleiro1 tabuleiro2)
	(equal tabuleiro1 tabuleiro2))


(defun tabuleiro->array (tabuleiro)
  (make-array '(18 10) :initial-contents tabuleiro))
  
  
(defun tabuleiro-limpa-linhas (tabuleiro)
	(let ((linhasRemovidas 0)
			(i 0))
		(loop (when (= i 18) (return linhasRemovidas))
			(cond 
				((tabuleiro-linha-completa-p tabuleiro i) (tabuleiro-remove-linha! tabuleiro i) (incf linhasRemovidas) (setf i 0))
				(T (incf i))))))                     
 

(defun array->tabuleiro (array)
	(let ((res (cria-tabuleiro)))
	(dotimes (i 18 res)
		(dotimes (j 10)
			(if (equal (aref array i j) T)
				(tabuleiro-preenche! res i j))))))
				

; funcoes 2.1.3 tipo estado ------------------------;


(defun copia-estado (e)
	(make-estado :pontos (estado-pontos e) :pecas-por-colocar (copy-list (estado-pecas-por-colocar e))
		:pecas-colocadas (copy-list (estado-pecas-colocadas e)) :tabuleiro (copia-tabuleiro (estado-tabuleiro e))))
		

(defun estados-iguais-p (e1 e2)
	(and (equal (estado-pontos e1) (estado-pontos e2))
		(equal (estado-pecas-por-colocar e1) (estado-pecas-por-colocar e2))
		(equal (estado-pecas-colocadas e1) (estado-pecas-colocadas e2))
		(equal (estado-tabuleiro e1)(estado-tabuleiro e2))))


(defun estado-final-p (e)
	(or (equal (estado-pecas-por-colocar e) NIL) (tabuleiro-topo-preenchido-p (estado-tabuleiro e))))
	

; funcoes 2.2.1 problema de procura ----------------;
	

(defun solucao (e)
	(and (equal (estado-pecas-por-colocar e) NIL) (equal (tabuleiro-topo-preenchido-p (estado-tabuleiro e)) NIL)))


(defun accoes (e)
	(let ((nextPeca NIL)
		(possiveisRot NIL)
		(listaRes NIL))
		(cond
			((equal (estado-final-p e) t) nil)
			(t (setf nextPeca (first (estado-pecas-por-colocar e)))
					(setf possiveisRot  (lista-rotacoes-pecas-possiveis nextPeca))
					(dolist (i possiveisRot listaRes)
						(dotimes (col (- 11 (array-dimension i 1)))
							(setf listaRes (append listaRes (list (cria-accao col i))))))))))


(defun lista-rotacoes-pecas-possiveis (peca)
	(cond 
		((equal peca 'I) (list peca-i0 peca-i1))
		((equal peca 'L) (list peca-l0 peca-l1 peca-l2 peca-l3))
		((equal peca 'J) (list peca-j0 peca-j1 peca-j2 peca-j3))
		((equal peca 'O) (list peca-o0))
		((equal peca 'S) (list peca-s0 peca-s1))
		((equal peca 'Z) (list peca-z0 peca-z1))
		((equal peca 'T) (list peca-t0 peca-t1 peca-t2 peca-t3))))
		

(defun qualidade (est)
	(* -1 (estado-pontos est)))


(defun custo-oportunidade (est)
	(let ((custo 0))
	(dolist (el (estado-pecas-colocadas est) custo)
		(cond
			((equal el 'I) (setf custo (+ custo pontuacao-max-peca-i)))
			((equal el 'L) (setf custo (+ custo pontuacao-max-peca-l)))
			((equal el 'J) (setf custo (+ custo pontuacao-max-peca-j)))
			((equal el 'O) (setf custo (+ custo pontuacao-max-peca-o)))
			((equal el 'S) (setf custo (+ custo pontuacao-max-peca-s)))
			((equal el 'Z) (setf custo (+ custo pontuacao-max-peca-z)))
			((equal el 'T) (setf custo (+ custo pontuacao-max-peca-t)))))
	(- custo (estado-pontos est))))


(defun resultado (est acc)
	(let ((peca nil)
			(hPeca nil)
			(wPeca nil)
			(colunaMaisAlta nil)
			(coluna nil)
			(alturaMax 0)
			(alturaMaxAux 0)
			(estNovo (copia-estado est)))
		(setf wPeca (array-dimension (accao-peca acc) 1))
		(setf hPeca (array-dimension (accao-peca acc) 0))
		(setf peca (accao-peca acc))
		(setf colunaMaisAlta 0)
		(setf coluna (accao-coluna acc))
		(dotimes (i  wPeca)
			(setf alturaMaxAux (tabuleiro-altura-coluna (estado-tabuleiro estNovo) (+ coluna i)))
			(cond
				((> alturaMaxAux alturaMax) (setf  alturaMax alturaMaxAux) (setf colunaMaisAlta (+ coluna i)))))
		(setf alturaMax (- alturaMax (1- hPeca)))
		(if (< alturaMax 0)
			(setf alturaMax 0))
		(loop
			(if (equal (conflitos (estado-tabuleiro estNovo) acc alturaMax) t)
				(incf alturaMax)
				(return nil)))
		(mete-peca (estado-tabuleiro estNovo) acc alturaMax)
		(setf (estado-pecas-colocadas estNovo) (append (list (first (estado-pecas-por-colocar estNovo))) (estado-pecas-colocadas estNovo)))
		(setf (estado-pecas-por-colocar estNovo) (rest (estado-pecas-por-colocar estNovo) ))
		(cond
		 	((not (tabuleiro-topo-preenchido-p (estado-tabuleiro estNovo))) (calcula-pontos estNovo)))
		estNovo))
	
	
(defun calcula-pontos (estado)
  (let ((nLinhas (tabuleiro-limpa-linhas (estado-tabuleiro estado))))
		(cond 
			((eql nLinhas 1) (setf (estado-pontos estado) (+ (estado-pontos estado) 100)))
			((eql nLinhas 2) (setf (estado-pontos estado) (+ (estado-pontos estado) 300)))
			((eql nLinhas 3) (setf (estado-pontos estado) (+ (estado-pontos estado) 500)))
			((eql nLinhas 4) (setf (estado-pontos estado) (+ (estado-pontos estado) 800))))))

	
(defun conflitos (tab acc alturaDeCheck)
	(let ((peca (accao-peca acc))
			(coluna (accao-coluna acc)))
		(dotimes (i (array-dimension peca 0) nil)
			(dotimes (j (array-dimension peca 1))
				(cond
					((or (> (+ alturaDeCheck i) 17) (> (+ coluna j) 9))	(return-from conflitos nil))
					
					((not (and (aref peca i j) (tabuleiro-preenchido-p tab (+ alturaDeCheck i) (+ coluna j))))
						
						(cond
							((and (not (equal i 0)) (or (equal peca peca-j1) (equal peca peca-j0) (equal peca peca-l0) (equal peca peca-l3) (equal peca peca-s0) 
								(equal peca peca-s1) (equal peca peca-z0) (equal peca peca-z1) (equal peca peca-t1) (equal peca peca-t2) (equal peca peca-t3)))
								
								(if (and (not (aref peca i j)) (tabuleiro-preenchido-p tab (+ alturaDeCheck i) (+ coluna j)) )
								 	(return-from conflitos t)))))
						
					(t (return-from conflitos t)))))))
						

(defun tipo-peca-problematica (tab peca coluna alturaDeCheck)
	(cond
		((or (equal peca peca-j1) (equal peca peca-j0) (equal peca peca-l0) (equal peca peca-l3) (equal peca peca-s0) (equal peca peca-s1)
			(equal peca peca-z0) (equal peca peca-z1) (equal peca peca-t1) (equal peca peca-t2) (equal peca peca-t3))
			
			(dotimes (i (array-dimension peca 0) nil)
				(dotimes (j (1- (array-dimension peca 1)))
					(if (and (aref peca i (1+ j)) (tabuleiro-preenchido-p tab (+ alturaDeCheck i) (+ coluna (1+ j))))
						(return-from tipo-peca-problematica t)))))))

		
(defun mete-peca (tab acc altura)
	(let ((peca (accao-peca acc))
			(coluna (accao-coluna acc)))
		(dotimes (i (array-dimension peca 0))
			(dotimes (j (array-dimension peca 1))
				(cond
					((or (> (+ altura i) 17) (> (+ coluna j) 9)) (return-from mete-peca nil))
					((aref peca i j) (tabuleiro-preenche! tab (+ altura i) (+ coluna j)) ))))))
	

; funcoes 2.2.2 procuras ---------------------------;


(defun procura-pp (prob)
	(labels ((procura-pp-aux (est prob a)
		(let ((child nil)
				(result nil)
				(rets nil))
			(cond
				((equal (funcall (problema-solucao prob) est) t) (list a) )
				(t (dolist (action (reverse (funcall (problema-accoes prob) est)) rets)
					(setf child (funcall (problema-resultado prob) est action))
					(setf result (procura-pp-aux child prob action))
					(if (equal (first result) action)
						(return (setf rets (append (list a) result))))))))))
		(rest (procura-pp-aux (problema-estado-inicial prob) prob nil))))


(defun cria-no (estado valor-f)
	(cons estado valor-f))


(defun no-estado (no)
	(car no))
	
	
(defun no-valor-f (no)
	(cdr no))


(defun set-no-valor-f (no val)
	(setf (cdr no) val))


(defun procura-A* (prob funcao)
	(labels ((procura-A*-aux (no-pai prob funcao f_limit acc)
		(let ((sucessores nil)
				(res-aux nil)
				(best nil)
				(alternative-f nil)
				(result nil)
				(aux nil)
				(valor-f nil)
				(alternative-aux nil))
			; chegamos a um estado solucao	
			(if (equal (funcall (problema-solucao prob) (no-estado no-pai)) t)
				(return-from procura-A*-aux (make-capsula :resultado (list acc) :valor-f (no-valor-f no-pai))))
			; gerar sucessores		
			(dolist (action (reverse (funcall (problema-accoes prob) (no-estado no-pai))))
				(setf res-aux (funcall (problema-resultado prob) (no-estado no-pai) action))
				(cond 
					((> (+(abs(funcall (problema-custo-caminho prob) res-aux)) (funcall funcao res-aux)) (abs (no-valor-f no-pai))) 
						(setf valor-f (+(funcall (problema-custo-caminho prob) res-aux)(funcall funcao res-aux))))
					(t (setf valor-f (no-valor-f no-pai))))0
				(setf sucessores (append sucessores (list	
					(make-sucessor :no-filho (cria-no res-aux valor-f) :accao action )))))
			; nao ha sucessores, logo falhou
			(if (equal sucessores nil)
				(return-from procura-A*-aux (make-capsula :resultado 'fail :valor-f infinito)))
			(loop
				(setf best (min-all sucessores))
				(if (> (no-valor-f (sucessor-no-filho best)) f_limit)
					(return-from procura-A*-aux (make-capsula :resultado 'fail :valor-f (no-valor-f (sucessor-no-filho best)))))	
			(cond
					((> (list-length sucessores) 1)
						(setf alternative-aux (min-all (remove best sucessores)))
						(if (equal alternative-aux nil)
							(return-from procura-A*-aux (make-capsula :resultado nil :valor-f infinito)))
						(setf alternative-f (no-valor-f (sucessor-no-filho alternative-aux)))) 
					(t (setf alternative-f infinito)))
				(setf aux (procura-A*-aux (sucessor-no-filho best) prob funcao (min f_limit alternative-f) (sucessor-accao best)))
				(setf result (capsula-resultado aux))
				(set-no-valor-f (sucessor-no-filho best) (capsula-valor-f aux))
				(if (not (equal result 'fail)) 
					(return-from procura-A*-aux (make-capsula :resultado (append (list acc) result) :valor-f (no-valor-f (sucessor-no-filho best)))))))))
		(rest (capsula-resultado 
					(procura-A*-aux 
						(cria-no 
							(problema-estado-inicial prob) (+(funcall (problema-custo-caminho prob) (problema-estado-inicial prob))(funcall funcao (problema-estado-inicial prob))))
						prob funcao infinito nil)))))


(defun min-all (l)
	(let ((res nil)
		(compare nil))
		(setf compare infinito)
		(dolist (el l res)
			(cond 
				((< (no-valor-f (sucessor-no-filho el)) compare) (setf compare (no-valor-f (sucessor-no-filho el))) (setf res el))))))


(defun procura-best (array pecas)
	(let ((est nil)
			(prob nil))
		(setf est (make-estado :pontos 0 :pecas-por-colocar pecas :tabuleiro (array->tabuleiro array)))
		(setf prob (make-problema :estado-inicial est :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade))
		(procura-A* prob #'heuristica-best)))


; funcoes heuristicas ------------------------------;


(defun heuristica-best (est)
	(+ (* coefH1 (heuristica-aplana est))  (* coefH2 (heuristica-uniformiza est))))


(defun heuristica-aplana(est)
	(let((res 0))
		(dotimes ( coluna 9 res)
			(setf res (+ res (tabuleiro-altura-coluna (estado-tabuleiro est) coluna))))))
	
	
(defun heuristica-uniformiza(est)
	(let ((res 0))
		(dotimes (coluna 8 res)
			(setf res (+ res (abs(- (tabuleiro-altura-coluna (estado-tabuleiro est) coluna) (tabuleiro-altura-coluna (estado-tabuleiro est) (+ coluna 1)))))))))




;---------------------------------------------------;


; importacao de bibliotecas-------------------------;
(load "utils.fas")
;---------------------------------------------------;
