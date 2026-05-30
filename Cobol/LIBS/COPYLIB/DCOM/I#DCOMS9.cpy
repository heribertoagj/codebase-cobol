      ******************************************************************
      * SISTEMA : DCOM - I#DCOMS9                                      *
      * TAMANHO : 0443 BYTES                                           *
      * ARQUIVO : TITULOS DESPREZADOS                                  *
      * INCLUDE DA TABELA DCOMB0K8 - DB2PRD.TDESPR_TITLO_OPER          *
      ******************************************************************
       01  S9-K8-REGISTRO.
 01 03     05  S9-K8-DANO-OPER-DESC            PIC S9(4)V COMP-3.
 04 05     05  S9-K8-NSEQ-OPER-DESC            PIC S9(9)V COMP-3.
 09 03     05  S9-K8-NTITLO-DESC-COML          PIC S9(5)V COMP-3.
 12 02     05  S9-K8-CBCO-PRODT-COBR           PIC S9(3)V COMP-3.
 14 02     05  S9-K8-CIDTFD-PRODT-COBR         PIC S9(2)V COMP-3.
 16 01     05  S9-K8-CIDTFD-PRODT-COBR-NULL    PIC  X(1).
 17 10     05  S9-K8-CNEGOC-COBR               PIC S9(18)V COMP-3.
 27 10     05  S9-K8-CTITLO-COBR-BCO           PIC S9(18)V COMP-3.
 37 02     05  S9-K8-CNRO-SEQ-TITLO            PIC S9(3)V COMP-3.
 39 06     05  S9-K8-NNOTA-PROMS-RURAL         PIC S9(11)V COMP-3.
 45 03     05  S9-K8-CINDCD-NOTA-PROMS         PIC X(3).
 48 02     05  S9-K8-CTPO-PROTR-RURAL          PIC S9(2)V COMP-3.
 50 02     05  S9-K8-CPRODT-RURAL              PIC S9(3)V COMP-3.
 52 05     05  S9-K8-QPRODT-RURAL              PIC S9(9)V COMP-3.
 57 02     05  S9-K8-CUND-MEDD                 PIC S9(3)V COMP-3.
 59 01     05  S9-K8-CINDCD-PGDOR-JURO         PIC X(1).
 60 10     05  S9-K8-DEMIS-TITLO               PIC X(10).
 70 10     05  S9-K8-DVCTO-TITLO-DESC          PIC X(10).
 80 09     05  S9-K8-VTITLO-DESC-COML          PIC S9(15)V9(2) COMP-3.
 89 05     05  S9-K8-CCNPJ-CPF                 PIC S9(9)V COMP-3.
 94 03     05  S9-K8-CFLIAL-CNPJ               PIC S9(5)V COMP-3.
 97 02     05  S9-K8-CCTRL-CNPJ-CPF            PIC S9(2)V USAGE COMP-3.
 99 60     05  S9-K8-IPSSOA-SACDO              PIC X(60).
159 15     05  S9-K8-CSEU-NRO-TITLO            PIC X(15).
174 02     05  S9-K8-CESPCE-TITLO-COBR         PIC S9(3)V COMP-3.
176 01     05  S9-K8-CINDCD-INSTR-DESC         PIC S9(1)V COMP-3.
177 03     05  S9-K8-CCEP                      PIC S9(5)V COMP-3.
180 02     05  S9-K8-CCEP-COMPL                PIC S9(3)V COMP-3.
182 30     05  S9-K8-ELOGDR                    PIC X(30).
212 40     05  S9-K8-IBAIRO                    PIC X(40).
252 30     05  S9-K8-IMUN                      PIC X(30).
282 02     05  S9-K8-CSGL-UF                   PIC X(2).
284 02     05  S9-K8-CSIT-DESC-COML            PIC S9(3)V COMP-3.
286 26     05  S9-K8-HSIT-DESC-COML            PIC X(26).
312 09     05  S9-K8-VABTMT-TITLO-DESC         PIC S9(15)V9(2) COMP-3.
321 09     05  S9-K8-VORPAG-RCBVL              PIC S9(15)V9(2) COMP-3.
330 03     05  S9-K8-NPCELA-DESC-COML          PIC S9(5)V COMP-3.
333 01     05  S9-K8-CINDCD-SELEC-DESC         PIC X(1).
334 26     05  S9-K8-HULT-ATULZ                PIC X(26).
360 05     05  S9-K8-CFUNC-BDSCO               PIC S9(9)V COMP-3.
365 08     05  S9-K8-CTERM                     PIC X(8).
373 10     05  S9-K8-DINCL-REG                 PIC X(10).
383 02     05  S9-K8-CORIGE-REC                PIC S9(3)V USAGE COMP-3.
385 02     05  S9-K8-CBCO-DEPOS                PIC S9(3)V USAGE COMP-3.
387 03     05  S9-K8-CAG-BCRIA-DEPOS           PIC S9(5)V USAGE COMP-3.
390 20     05  S9-K8-RANLSE-CREDT-CLI          PIC X(20).
410 10     05  S9-K8-DSOLTC-ANLSE-CREDT        PIC X(10).
420 02     05  S9-K8-CSIT-ANLSE-CREDT          PIC S9(3)V USAGE COMP-3.
422 01     05  S9-K8-CINDCD-ISENC-ANLSE        PIC X(1).
423 01     05  S9-K8-CINDCD-PROTE-TITLO        PIC X(1).
424 02     05  S9-K8-QDIA-PROTE-TITLO          PIC S9(3)V USAGE COMP-3.
426 02     05  S9-K8-CBCO-DEB-AUTOM            PIC S9(3)V USAGE COMP-3.
428 03     05  S9-K8-CAG-DEB-AUTOM             PIC S9(5)V USAGE COMP-3.
431 07     05  S9-K8-CCTA-DEB-AUTOM            PIC S9(13)V USAGE COMP-3.
438 02     05  S9-K8-CIDTFD-PRODT-ORIGN        PIC S9(2)V USAGE COMP-3.
440 05     05  S9-K8-CADM-CATAO-CREDT          PIC S9(9)V USAGE COMP-3.
445 01     05  S9-K8-CINDCD-VDA-RCBVL          PIC  X(1).

