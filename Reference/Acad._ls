(defun DXF (code elist)
  (cdr (assoc code elist))
);dxf

(setvar "CMDECHO" 0)

;--------------LOAD 1 SO FILE LSP THIET YEU-------------------

(load "text-uti.lsp")
(load "dim-uti.lsp")
(load "BLOCK_uti.lsp")
(load "VETD-A3.lsp")
(load "IN-A3-2004.lsp")
(load "pload.lsp")
(load "TNQL1.lsp")
(load "Tinhtoan.lsp")
(load "VBA.lsp")
(load "vegiengthu.lsp")
;--------------------------------------------------------------


(defun c:CHT()
(load "chtext.lsp"))
(defun c:acad()
(load "acad.lsp"))
(defun c:ark()
(load "dtp.lsp"))
(defun c:kb()
(load "kb1.lsp"))

(defun c:eall()
(load "eRASE1.lsp"))
;(defun c:FF()
;(load "fREEZE.lsp"))

(defun c:uti()
   (load "uti.lsp"))

(defun c:kh()
   (load "KH_Dim_up.lsp"))

;(defun c:TUP()
;  (load "text-up.lsp"))
(defun c:TU()
  (load "text-uti.lsp"))
(defun c:DU()
  (load "dim-uti.lsp"))
(defun c:THU()
   (load "THU.lsp")
(princ))
(defun c:df()
  (load "dist1.lsp"))
(defun c:DTHH()
  (load "dthh.lsp"))
(defun c:fg()
  (load "fg.lsp"))

(defun c:TALUY()
  (load "TALUY.lsp"))

(defun c:PRO()
  (load "PROFILE5.lsp"))

(defun c:sta()
(load "station.lsp"))

(defun c:COO()
  (load "COO_SCR.lsp")
(PRINC))

(defun c:BU()
  (load "BLOCK_uti.lsp"))

(defun c:CLO()
  (load "CLOT2.lsp"))

(defun c:CIR()
  (load "CH-cir.lsp"))

(defun c:IS()
  (load "IRshape1.lsp"))

(defun c:ttr()
  (command "CIRCLE" "TTR"))

(defun deflt (str1 def)
   (strcat Str1 "<" (rtos def 2 3 ) ">: ")
);defun

(DEFUN c:HP()
 (load "HP.lsp")
 (princ)
);DEFUN

(defun c:TL()
   (setvar "REGENMODE" 0)
   (if (/= scale nil)
      (setq *SCALE (getreal (deflt "\nInput current scale: " scale)))
      (setq *SCALE (getreal (deflt "\nInput current scale: " 0.0)))
   )
   (if (not *scale) (setq *scale scale) (setq scale *scale))
   (command "STYLE" "5" "Vni-HELVE" (* 3.5 SCALE) "1" "0" "N" "N")
   (command "STYLE" "3" "Vni-HELVE" (* 2.5 SCALE) "1" "0" "N" "N")
   (command "STYLE" "2" "VN_vni.SHX" (* 1.6 SCALE) "0.7" "0" "N" "N" "N")
   (command "STYLE" "DIMTEXT" "VN_vni.SHX" "0" "0.7" "0" "N" "N" "N") ; dung cho dimension
   (command "STYLE" "DIMTEXT1" "Vni-HELVE" (* 2.0 SCALE) "1.0" "0" "N" "N") ; dung cho dimension  
   (command "STYLE" "2d" "Vni-HELVE" (* 2.0 SCALE) "1.0" "0" "N" "N")
   (setvar "DIMCLRE" 2)      		;Color
   (setvar "DIMCLRD" 2)      		;Color
   (setvar "DIMCLRT" 4)      		;Color
   (setvar "DIMZIN" 2)       		;Trailing
   (setvar "DIMSAH" 0)       		;Trailing
   (setvar "DIMEXE" 1.0)              	;Extension above line
   (setvar "DIMBLK" ".")              	;Default block
   (setvar "DIMEXO" 0)                	;Feature offset
   (setvar "DIMASZ" 1.6)              	;Arrow size 
   (setvar "DIMTSZ" 0)                	;Tick size 
   (setvar "DIMDLE" (* 0.2 1))        	;Tick extension 
   (setvar "DIMTXT" (* 1.6 1))        	;Text height
   (setvar "DIMGAP" (* 0.6 1))        	;Text gap
   (setvar "DIMTAD" 1)                	;Verical (defauft)
   (setvar "DIMTIH" 0)                	;Horizontal
   (setvar "DIMTOH" 0)                	;Horizontal
   (setvar "DIMTOFL" 1)               	;Force line inside
   (setvar "DIMTIX" 1)                	;Keep text between Ext. lines
   (setvar "DIMTXSTY" "DIMTEXT")      	;Text STYLE
   (setvar "DIMLFAC" 10)              	;LINEAR FACTOR
   (setvar "DIMFIT" 5)                	;No LEADER
   (setvar "DIMSCALE" SCALE)          	;Overall Scale
   (setq tendim (rtos (* 10 scale) 2 0))
   (if (= nil (tblsearch "dimstyle" tendim))
      (command "-dimstyle" "save" tendim) 
      (command "-dimstyle" "save" tendim "Y") 
;      (command "-dimstyle" "restore" tendim) 
   );if

   (setvar "REGENMODE" 1)
)
(defun c:BC()
   (setvar "REGENMODE" 0)
   (if (/= scale nil)
      (setq *SCALE (getreal (deflt "\nInput current scale: " scale)))
      (setq *SCALE (getreal (deflt "\nInput current scale: " 0.0)))
   )
   (if (not *scale) (setq *scale scale) (setq scale *scale))
   (command "STYLE" "5" "Vni-HELVE" (* 3.5 SCALE) "1" "0" "N" "N")
   (command "STYLE" "3" "Vni-HELVE" (* 2.5 SCALE) "1" "0" "N" "N")
   (command "STYLE" "2" "VN_vni.SHX" (* 1.6 SCALE) "0.8" "0" "N" "N" "N")
   (command "STYLE" "DIMTEXT1" "Vni-HELVE" (* 2.0 SCALE) "1.0" "0" "N" "N") ; dung cho dimension  
   (command "STYLE" "2d" "Vni-HELVE" (* 2.0 SCALE) "1.0" "0" "N" "N")
   (setvar "DIMCLRE" 6)      		;Color
   (setvar "DIMCLRD" 6)      		;Color
   (setvar "DIMCLRT" 7)      		;Color
   (setvar "DIMZIN" 2)       		;Trailing
   (setvar "DIMSAH" 0)       		;Trailing
   (setvar "DIMEXE" 1.0)              	;Extension above line
   (setvar "DIMBLK" ".")              	;Default block
   (setvar "DIMEXO" 0)                	;Feature offset
   (setvar "DIMASZ" 1.6)              	;Arrow size 
   (setvar "DIMTSZ" 0)                	;Tick size 
   (setvar "DIMDLE" (* 0.2 1))        	;Tick extension 
   (setvar "DIMTXT" (* 1.6 1))        	;Text height
   (setvar "DIMGAP" (* 0.6 1))        	;Text gap
   (setvar "DIMTAD" 1)                	;Verical (defauft)
   (setvar "DIMTIH" 0)                	;Horizontal
   (setvar "DIMTOH" 0)                	;Horizontal
   (setvar "DIMTOFL" 1)               	;Force line inside
   (setvar "DIMTIX" 1)                	;Keep text between Ext. lines
   (setvar "DIMTXSTY" "DIMTEXT1")      	;Text STYLE
   (setvar "DIMLFAC" 1)              	;LINEAR FACTOR
   (setvar "DIMFIT" 5)                	;No LEADER
   (setvar "DIMSCALE" SCALE)          	;Overall Scale
   (setq tendim (rtos (* 10 scale) 2 0))
   (if (= nil (tblsearch "dimstyle" tendim))
      (command "-dimstyle" "save" tendim) 
      (command "-dimstyle" "save" tendim "Y") 
;      (command "-dimstyle" "restore" tendim) 
   );if

   (setvar "REGENMODE" 1)
)

(defun c:TLCM() ;VE TILE CENTIMET
   (setvar "REGENMODE" 0)
   (if (/= scale nil)
      (setq *SCALE (getreal (deflt "\nInput current scale: " scale)))
      (setq *SCALE (getreal (deflt "\nInput current scale: " 0.0)))
   )
   (if (not *scale) (setq *scale scale) (setq scale *scale))
   (command "STYLE" "5" "Vni-Helve" (* 3.5 SCALE) "1" "0" "N" "N")
   (command "STYLE" "3" "Vni-Helve" (* 2.5 SCALE) "1" "0" "N" "N")
;   (command "STYLE" "V2" "VN_vni.SHX" (* 1.8 SCALE) "0.7" "0" "N" "N" "N")
   (command "STYLE" "2" "VN_vni.SHX" (* 1.8 SCALE) "0.7" "0" "N" "N" "N")
   (command "STYLE" "2d" "Vni-Helve" (* 2.0 SCALE) "1.0" "0" "N" "N")
   (setvar "DIMCLRE" 256)      ;Color
   (setvar "DIMCLRD" 256)      ;Color
   (setvar "DIMCLRT" 256)      ;Color
   (setvar "DIMZIN" 8)       ;Trailing
   (setvar "DIMSAH" 0)       ;Trailing
   (setvar "DIMEXE" SCALE)             ;Extension above line
   (setvar "DIMBLK" ".")               ;Default block
   (setvar "DIMEXO" 0)                 ;Feature offset
   (setvar "DIMASZ" SCALE)             ;Arrow size 
   (setvar "DIMTSZ" 0)                 ;Tick size 
   (setvar "DIMDLE" (* 0.2 SCALE))     ;Tick extension 
   (setvar "DIMTXT" (* 1.8 SCALE))     ;Text height
   (setvar "DIMGAP" (* 0.6 SCALE))     ;Text gap
   (setvar "DIMTAD" 1)                 ;Verical (defauft)
   (setvar "DIMTIH" 0)                 ;Horizontal
   (setvar "DIMTOH" 0)                 ;Horizontal
   (setvar "DIMTOFL" 1)                ;Force line inside
   (setvar "DIMTXSTY" "2")            ;Text STYLE
   (setvar "DIMLFAC" 10)              ;LINEAR FACTOR
   (setvar "DIMFIT" 5)                ;No LEADER
   (setvar "REGENMODE" 1)
)

(defun c:TLM() ;VE TILE MET
   (setvar "REGENMODE" 0)
   (if (/= scale nil)
      (setq *SCALE (getreal (deflt "\nInput current scale: " scale)))
      (setq *SCALE (getreal (deflt "\nInput current scale: " 0.0)))
   )
   (if (not *scale) (setq *scale scale) (setq scale *scale))
   (command "STYLE" "5" "Vni-Helve" (* 3.5 SCALE) "1" "0" "N" "N")
   (command "STYLE" "3" "Vni-Helve" (* 2.5 SCALE) "1" "0" "N" "N")
;   (command "STYLE" "V2" "VN_vni.SHX" (* 1.8 SCALE) "0.7" "0" "N" "N" "N")
   (command "STYLE" "2" "VN_vni.SHX" (* 1.8 SCALE) "0.7" "0" "N" "N" "N")
   (command "STYLE" "2d" "Vni-Helve" (* 2.0 SCALE) "1.0" "0" "N" "N")
   (setvar "DIMCLRE" 256)      ;Color
   (setvar "DIMCLRD" 256)      ;Color
   (setvar "DIMCLRT" 256)      ;Color
   (setvar "DIMZIN" 8)       ;Trailing
   (setvar "DIMSAH" 0)       ;Trailing
   (setvar "DIMEXE" SCALE)             ;Extension above line
   (setvar "DIMBLK" ".")               ;Default block
   (setvar "DIMEXO" 0)                 ;Feature offset
   (setvar "DIMASZ" SCALE)             ;Arrow size 
   (setvar "DIMTSZ" 0)                 ;Tick size 
   (setvar "DIMDLE" (* 0.2 SCALE))     ;Tick extension 
   (setvar "DIMTXT" (* 1.8 SCALE))     ;Text height
   (setvar "DIMGAP" (* 0.6 SCALE))     ;Text gap
   (setvar "DIMTAD" 1)                 ;Verical (defauft)
   (setvar "DIMTIH" 0)                 ;Horizontal
   (setvar "DIMTOH" 0)                 ;Horizontal
   (setvar "DIMTOFL" 1)                ;Force line inside
   (setvar "DIMTXSTY" "2")            ;Text STYLE
   (setvar "DIMLFAC" 10)              ;LINEAR FACTOR
   (setvar "DIMFIT" 5)                ;No LEADER
   (setvar "REGENMODE" 1)
)

(defun c:acc ()
(setq #acc (getint "\nDo Chinh Xac: "))
(setvar "DIMDEC" #ACC))

(SETQ #ACC 1)
(defun c:bac () ; DO CHINH XAC CUA CD BLOCK
   (setq #Bacc (getint "\nDo Chinh Xac cua CD block: "))
)

(SETQ #ACC 1)

(defun c:3p()
(command "UCS" "3p"))

(DEFUN C:WW() 
(COMMAND "ucs" "W"))

(DEFUN C:OO() 
   (SETQ PT (GETPOINT "\nPick (0,0) point"))
(COMMAND "ucs" "O" PT))

(defun c:T2() (setvar "TEXTSTYLE" "2")
(command "DTEXT"))


(defun c:T3() (setvar "TEXTSTYLE" "3")
(command "DTEXT" "j" "c"))

(defun c:T5() (setvar "TEXTSTYLE" "5")
(command "DTEXT" "j" "c"))


(defun c:MT() (setvar "MIRRTEXT" 0 ))

(defun c:UB() (command "UNDO" "B"))

(DEFUN C:ax() 
   (command "setvar" "OSMODE" "33")
   (COMMAND "redraw")
(COMMAND "area"))
(defun c:L1() (command "LAYER" "N" "1" "C" "1" "1" ""))
(defun c:L2() (command "LAYER" "N" "2" "C" "2" "2" ""))
(defun c:L4() 
   (if (= nil (tblsearch "ltype" "DASHED"))
      (command "-LINETYPE" "L" "DASHED" "" "")
   );if
(command "LAYER" "N" "4" "C" "4" "4" "Lt" "DASHED" "4" ""))
(defun c:L3() (command "LAYER" "N" "3" "C" "3" "3" ""))
(defun c:L5() (command "LAYER" "N" "5" "C" "5" "5" ""))
(defun c:L9() (command "LAYER" "N" "9" "C" "9" "9" ""))
(defun c:L7() (command "LAYER" "N" "7" "C" "7" "7" ""))
(defun c:L8() (command "LAYER" "N" "8" "C" "8" "8" ""))
(defun c:L6()
   (if (= nil (tblsearch "ltype" "CENTER"))
      (command "-LINETYPE" "L" "CENTER" "" "")
   );if
(command "LAYER" "N" "6" "C" "6" "6" "Lt" "CENTER" "6" ""))
(defun c:0() (command "LAYER" "S" "0" ""))
(defun c:1() (command "LAYER" "S" "1" ""))
(defun c:2() (command "LAYER" "S" "2" ""))
(defun c:3() (command "LAYER" "S" "3" ""))
(defun c:4() (command "LAYER" "S" "4" ""))
(defun c:5() (command "LAYER" "S" "5" ""))
(defun c:6() (command "LAYER" "S" "6" ""))
(defun c:7() (command "LAYER" "S" "7" ""))
(defun c:8() (command "LAYER" "S" "8" ""))
(defun c:9() (command "LAYER" "S" "9" ""))

(defun c:0() (command "LAYER" "T" "0" "S" "0" ""))
(defun c:1() (command "LAYER" "T" "1" "S" "1" ""))
(defun c:2() (command "LAYER" "T" "2" "S" "2" ""))
(defun c:3() (command "LAYER" "T" "3" "S" "3" ""))
(defun c:4() (command "LAYER" "T" "4" "S" "4" ""))
(defun c:5() (command "LAYER" "T" "5" "S" "5" ""))
(defun c:6() (command "LAYER" "T" "6" "S" "6" ""))
(defun c:7() (command "LAYER" "T" "7" "S" "7" ""))
(defun c:8() (command "LAYER" "T" "8" "S" "8" ""))
(defun c:9() (command "LAYER" "T" "9" "S" "9" ""))

(defun c:c0() (SSGET ) (command ".chprop" "p" "" "la" "0" "LT" "BYLAYER" "C" "BYLAYER" ""))
(defun c:c1() (SSGET) (command ".chprop" "p" "" "la" "1" "LT" "BYLAYER" "C" "BYLAYER" ""))
(defun c:c2() (SSGET) (command ".chprop" "p" "" "la" "2" "LT" "BYLAYER" "C" "BYLAYER" ""))
(defun c:c3() (SSGET) (command ".chprop" "p" "" "la" "3" "LT" "BYLAYER" "C" "BYLAYER" ""))
(defun c:c4() (SSGET) (command ".chprop" "p" "" "la" "4" "LT" "BYLAYER" "C" "BYLAYER" ""))
(defun c:c5() (SSGET) (command ".chprop" "p" "" "la" "5" "LT" "BYLAYER" "C" "BYLAYER" ""))
(defun c:c6() (SSGET) (command ".chprop" "p" "" "la" "6" "LT" "BYLAYER" "C" "BYLAYER" ""))
(defun c:c7() (SSGET) (command ".chprop" "p" "" "la" "7" "LT" "BYLAYER" "C" "BYLAYER" ""))
(defun c:c8() (SSGET) (command ".chprop" "p" "" "la" "8" "LT" "BYLAYER" "C" "BYLAYER" ""))
(defun c:c9() (SSGET) (command ".chprop" "p" "" "la" "9" "LT" "BYLAYER" "C" "BYLAYER" ""))

(defun c:f0() (command "LAYER" "F" "0" ""))
(defun c:f1() (command "LAYER" "F" "1" ""))
(defun c:f2() (command "LAYER" "F" "2" ""))
(defun c:f3() (command "LAYER" "F" "3" ""))
(defun c:f4() (command "LAYER" "F" "4" ""))
(defun c:f5() (command "LAYER" "F" "5" ""))
(defun c:f6() (command "LAYER" "F" "6" ""))
(defun c:f7() (command "LAYER" "F" "7" ""))
(defun c:f8() (command "LAYER" "F" "8" ""))
(defun c:f9() (command "LAYER" "F" "9" ""))

;(defun c:SC() (command "SCALE"))
(defun c:DT() (command "DTEXT"))
(defun c:MV() (command "MVIEW"))
(setq *MA 300.0)
(defun c:MALL() 
   (setq MA (getreal (deflt "\nInput the moving distance: " *MA)))
   (if (not MA) (setq MA *MA) (setq *MA *MA))
   (command "Move" "all"  "" (strcat (rtos MA 2 10) ",0") "")
(princ)
)
(defun c:MN() 
   (command "Move" "all"  ""  "300,0" "")
(princ)
)
(defun c:DO() (command "DONUT"))
(defun c:B() (command "BLOCK"))
(defun c:CB() (command "COPYBASE"))
(defun c:PG() (command "pagesetup"))
(defun c:C() (command "COPY"))
(defun c:CI() (command "CIRCLE"))
(defun c:CIT() (command "CIRCLE" "TTR"))
(defun c:CIP() (command "CIRCLE" "2P"))
;(defun c:SA() (command "QSAVE"))
(defun c:IN() (command "OSNAP" "INT"))
(defun c:MID() (command "OSNAP" "MID"))
(defun c:EN() (command "OSNAP" "END"))
(defun c:CEN() (command "OSNAP" "CEN"))
(defun c:NO() (command "OSNAP" "NON"))
(defun c:OS() (command "OSNAP" "END,INT"))
(defun c:RE() (command "REGEN"))
(defun c:AR() (command "ARRAY"))
(defun c:RO() (command "ROTATE"))
(defun c:FD() (command "FIND"))
(defun c:RY() (command "RAY"))
(defun c:PE() (command "PEDIT"))
(defun c:lo() (c:layon))
(defun c:ff() (c:layoff))
(defun c:lc() (c:laycur))
(defun c:ol() (c:layiso))
(defun c:lf() (c:layfrz))
(defun c:QS() (command "PSPACE"))
(defun c:AS() (command "MSPACE"))
(defun c:P() (command "PAN"))
(defun c:O() (command "OFFSET"))
(defun c:CF() (command "CHAMFER"))
(defun c:T() (command "TRIM"))
(defun c:F() (command "FILLET"))
(defun c:MI() 
   (setvar "MIRRTEXT" 0) 
(command "MIRROR"))
(defun c:ZA() (command "ZOOM" "A"))
(defun c:ZE() (command ".ZOOM" "E"))
(defun c:ZZ() (command ".ZOOM" "0.5x"))
(defun c:Z+() (command ".ZOOM" "2.0x"))
(defun c:Z++() (command ".ZOOM" "2.0x"))
(defun c:ZZZ() (command ".ZOOM" "5x"))
(defun c:ZW() (command "ZOOM" "W"))
(defun c:ZQ() (command "ZOOM" "P"))
(defun c:ZP() (command "ZOOM" "P"))
(defun c:ZD() (command "ZOOM" "D"))
(defun c:ZV() (command "ZOOM" "V"))
;(defun c:CH() (command "CHANGE"))
;(defun c:CHP() (command "CHPROP"))
(defun c:XP() (command "EXPLODE"))
(defun c:LI() (command "LIST"))
(defun c:DI() (command "DIST"))
(defun c:EX() (command "EXTRIM"))
(defun c:I() (command "INSERT"))
(defun c:WB() (command "WBLOCK"))
(defun c:VP() (command "VPOINT" "R"))
(defun c:EX() (command "EXTEND"))
(defun c:S() (command "STRETCH" "C"))
(defun c:PU()
   (command "purge" "a" "")
   (while (= (getvar "CMDNAMES") "PURGE")
      (command "y" )
   );end while
)

(defun c:LTS (/ TILE )
   (setq TILE (getvar "TILEMODE"))
   (if (= #LTS_PS nil) (setq #LTS_PS 1))
   (if (= #LTS_MS nil) (setq #LTS_MS 1))
   (if (= TILE 0) 
      (progn 
         (setq LTS_PS (getREAL (strcat "\nInpput LTSCALE factor for PAPER SPACE "
         "<" (rtos #LTS_PS 2 2) ">"))) 
         (if (not LTS_PS) (setq LTS_PS #LTS_PS) (setq #LTS_PS LTS_PS))
         (setvar "LTSCALE" LTS_PS)
         (command "REGENALL")
      ) 
      (progn 
         (setq #LTS_MS (getreal (STRCAT "\nInpput LTSCALE factor for MODEL SPACE "
         "<" (rtos #LTS_MS 2 2) ">"))) 
         (if (not LTS_MS) (setq LTS_MS #LTS_MS) (setq #LTS_MS LTS_MS))
         (setvar "LTSCALE" LTS_MS)
         (command "REGENALL")
      ) 
   )
)
(defun c:TM( / TILE)
 (setq TILE (getvar "TILEMODE")) 
 (if (= TILE 0)
    (progn
      (setvar "TILEMODE" 1)
    )      
    (progn
      (setvar "TILEMODE" 0)
    )
 );if
)
(defun c:ED() (command "DDEDIT"))
(defun c:UP() (command "DIM1" "UP"))
(defun c:DED() (command "DIM1" "NEW"))
(defun c:DM() (command "dim1" "tedit"))
(defun c:AN() (command "dim1" "AN"))
(defun c:RA() (command "dim1" "RA"))
(defun c:DLi() (command "dim1" "ALI"))
(defun c:EDD() (COMMAND "DDATTE"))
(defun c:MO()	(command "_properties"))
(defun c:A1() (command "PLINE" "0,0" "817,0" "817,570" "0,570" "c"))
(defun c:A2() (command "PLINE" "0,0" "570,0" "570,396" "0,396" "c"))
(defun c:A3() (command "PLINE" "0,0" "396,0" "396,273" "0,273" "c"))
(defun c:A4() (command "PLINE" "0,0" "198,0" "198,285" "0,285" "c"))
(defun c:A4N() (command "PLINE" "0,0" "285,0" "285,198" "0,198" "c"))
(defun c:AA3() (command "PLINE" "20.38,1.5" "20.38,1.5" "400.5,1.5" "400.5,274.5" "20.38,274.5" "c"))

(defun c:ALI( / pt1 pt2 pt3 pt4 pt5 pt11 dis olast)
   (prompt "\nNew align dim method.")
   (setq OLAST (getvar "OSMODE"))
   (setq LLAST (getvar "CLAYER"))   
   (if (= nil (tblsearch "layer" "2"))
    (progn
      (C:L2)
      (setvar "CLAYER" "2")
     );progn
      (setvar "CLAYER" "2")
   );if

   (setq #DIM 3)
   (setq pt1 (getpoint "\nSelect first extension line origin : ")
      pt2 (getpoint "\nSelect first extension line origin : " pt1)
      pt3 (getpoint "\nExtension line origin location : ")
      #A (angle pt1 pt2)
      pt4 (polar pt3 (+ #A 1.57076) 100)
      pt5 (inters pt1 pt2 pt3 pt4 seg)
      dis (distance pt3 pt5)
      pt11 (polar pt1 (+ #A 1.57076) dis)  
      #pt22 (polar pt2 (+ #A 1.57076) dis)  
   )
   (SETVAR "OSMODE" 0)
   (command "dim1" "ali" pt11 #pt22)
   (setvar "OSMODE" OLAST)
   (SETVAR "CLAYER" LLAST)   
) 
;(prompt "\n ALI to use new ALI dim method")
;----------------------------------------------------------
(defun c:MT()
	(if (= scale nil)  (setq scale (getreal "\nInput current scale: ")))
	
	(setq pt1 (getpoint "\nPick the insert point :"))
        (setq pt2 (getpoint "Pick the second point for rotate arrow :" pt1))
	(setq ang (/ (* (angle pt1 pt2) 180) pi))
	(command "INSERT" "MT" pt1 scale scale ang)
);defun
;----------------------------------------------------------
(defun c:FN( / fn last pt dtime yy mm dd h m ed new old newlay oldlay)
   (setq fn (strcat (getvar "dwgPREFIX") (getvar "dwgNAME")))
   ;(command "_qsave")
   ;(setq fN (getvar "SAVENAME"))
   (setq ten (getvar "LOGINNAME"))
   (setq oLAST (getvar "OSMODE"))
   ;(setq fn (strcat FP fn))
   ;(setq fm (strcat "Ten file la :" fn))
   ;(princ fm)
   ;(princ "\n")
   (setq dat (getvar "cdate"))
   (setq date (rtos dat 2 8))
   ;(princ date)
   ;(princ "\n")
   (setq yy (substr date 3 2))
   (setq mm (substr date 5 2))
   (setq dd (substr date 7 2))
   (setq h (substr date 10 2))
   (setq m (substr date 12 2))
   (setq pt (getpoint "Diem viet ten file :"))
   ;(command "text" "style" "kt" pt "2" "0" fn)
   (setq dtime (strcat dd "/" mm "/" yy "-" h ":" m))
   (command "OSMODE" "0")
   (command "text" "style" "2"  (list (- ( car pt) 8) (cadr pt)) "90" (strcat fn "-" dtime "- by " ten))
;   (command "text" "style" "2"  (list (- ( car pt) 10) (+ (cadr pt) 0)) "2" "90" (strcat fn "-" dtime "- by " ten))
;   (setq ed (entget (entlast)))
;   (setq old (cons 40 (DXF 40 ed))
;      new (cons 40 1.2)
;      ed (subst new old ed)
;   );setq
;   (setq oldlay (cons 8 (DXF 8 ed))
;      newlay (cons 8 "6")
;      ed (subst newlay oldlay ed)
;   );setq
;   (entmod ed) 
   (princ)
   (command "OSMODE" OLAST)   
);defun FN
(defun C:TG ( / ssl  nsset temp ed)
     (setq #sset (ssget))
     (setq ssl (sslength #sset) 
           nsset (ssadd)
     )
     (print ssl)
     (princ "entities found. ") 
     (princ "\nVerifying the selected entities -- please wait. ") 
     (setq pathtemp "D:\\HIEN\\CONGTRINH\\TEMP\\")
;     (setq filetemp "temp.txt")
     (if (not *fname) (setq *fname "temp.txt"))
     (setq filetemp (getstring (strcat "\nTen tap tin<" *fname ">:")))
     (if (= filetemp "") (setq filetemp *fname) (setq *fname filetemp))
     (setq ff (open (strcat pathtemp filetemp) "a"))
     (princ "\n" ff)
     (while (> ssl 0)
          (progn
               (setq temp (ssname #sset (setq ssl (1- ssl))))
               (setq ed (entget temp))
               (if (= (DXF 0 ed) "TEXT") (ssadd temp nsset))
;               (setq No (atof (DXF 1 ed)))
               (setq No (DXF 1 ed))
               ;(setq conts (strcat conts " " No))
               ;(if (= (/ ssl 4) 0) (print "*" ff))
               (princ (strcat " " No) ff)
               (princ (strcat "\n" No))
          )
     )
     (setq ssl (sslength nsset)
          #sset nsset
     )
     (close ff)
     (princ)
);defun Text_get
;----------------------------------------------------------
(princ "\n")
(princ "\n CAD Utilities (Written by NOBODY)")
(princ "\n              JULY 20th '2006 ")
(princ "\n    Last modification on AUGUST '06 ")
(princ)

