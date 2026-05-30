      ******************************************************************
      * SISTEMA : DCOM - I#DCOMWA                                      *
      * TAMANHO : 0700 BYTES                                           *
      * ARQUIVO : ARQUIVO INTERFACE SISTEMA WARE                       *
      *           CONTEM A PARCELA COMPLETA (C/ SACADO E OPERACAO)     *
      ******************************************************************
BSI001*                    ALTERACAO - BSI TECNOLOGIA                  *
BSI001*----------------------------------------------------------------*
BSI001*  ANALISTA BSI:   FERNANDO LUIZ DE SANTI  -  BSI TECNOLOGIA     *
BSI001*  DATA........:   28/04/2017                                    *
BSI001*----------------------------------------------------------------*
BSI001*  OBJETIVO....:   INCLUIR CAMPOS DE ENCARGOS MORATORIOS.        *
BSI001******************************************************************
       01  WA-C9-REGISTRO.
  1  3     05 WA-C9-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
  4  5     05 WA-C9-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
  9  3     05 WA-C9-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
 12  2     05 WA-C9-DMES-VCTO-PTCAO      PIC S9(2)V USAGE COMP-3.
 14  2     05 WA-C9-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
 16 26     05 WA-C9-HSIT-DESC-COML       PIC X(26).
 42 10     05 WA-C9-DVCTO-PCELA-DESC     PIC X(10).
 52  9     05 WA-C9-VPRINC-PCELA-DESC    PIC S9(15)V9(2) USAGE COMP-3.
 61  5     05 WA-C9-QTITLO-CHEQ-PCELA    PIC S9(9)V USAGE COMP-3.
 66  5     05 WA-C9-PTX-JURO-MES         PIC S9(3)V9(5) USAGE COMP-3.
 71  5     05 WA-C9-PTX-JURO-ANO         PIC S9(3)V9(5) USAGE COMP-3.
 76  5     05 WA-C9-PTX-CUSTO-MES        PIC S9(3)V9(6) USAGE COMP-3.
 81  5     05 WA-C9-PTX-CUSTO-ANO        PIC S9(3)V9(6) USAGE COMP-3.
 86  9     05 WA-C9-VCUSTO-PCELA-DESC    PIC S9(15)V9(2) USAGE COMP-3.
 95  9     05 WA-C9-VJURO-PCELA-DESC     PIC S9(15)V9(2) USAGE COMP-3.
104  9     05 WA-C9-VJURO-DIA-PCELA      PIC S9(15)V9(2) USAGE COMP-3.
113  9     05 WA-C9-VIOF-PCELA-DESC      PIC S9(15)V9(2) USAGE COMP-3.
122  9     05 WA-C9-VPSENT-PCELA-DESC    PIC S9(15)V9(2) USAGE COMP-3.
131  9     05 WA-C9-VJURO-APROP-ACUM     PIC S9(15)V9(2) USAGE COMP-3.
140 10     05 WA-C9-DULT-APROP-PCELA     PIC X(10).
150  5     05 WA-C9-PALIQT-IOF-PRROG     PIC S9(3)V9(5) USAGE COMP-3.
155  9     05 WA-C9-VIOF-PRROG-PCELA     PIC S9(15)V9(2) USAGE COMP-3.
164  5     05 WA-C9-PCUSTO-MES-MORA      PIC S9(3)V9(6) USAGE COMP-3.
169  5     05 WA-C9-PCUSTO-ANO-MORA      PIC S9(3)V9(6) USAGE COMP-3.
174  5     05 WA-C9-PCUSTO-MES-CRATS     PIC S9(3)V9(6) USAGE COMP-3.
179  5     05 WA-C9-PCUSTO-ANO-CRATS     PIC S9(3)V9(6) USAGE COMP-3.
184  5     05 WA-C9-PALIQT-IOF-VENCD     PIC S9(3)V9(5) USAGE COMP-3.
189  9     05 WA-C9-VIOF-PCELA-VENCD     PIC S9(15)V9(2) USAGE COMP-3.
198  9     05 WA-C9-VJURO-MORA-GERC      PIC S9(15)V9(2) USAGE COMP-3.
207  9     05 WA-C9-VJURO-MORA-LEGAL     PIC S9(15)V9(2) USAGE COMP-3.
216  9     05 WA-C9-VSDO-DVDOR-PRINC     PIC S9(15)V9(2) USAGE COMP-3.
225  9     05 WA-C9-VCUSTO-PCELA-VENCD   PIC S9(15)V9(2) USAGE COMP-3.
234  9     05 WA-C9-VJURO-TRNSF-CRATS    PIC S9(15)V9(2) USAGE COMP-3.
243  9     05 WA-C9-VCUSTO-TRNSF-CRATS   PIC S9(15)V9(2) USAGE COMP-3.
252 10     05 WA-C9-DTRNSF-CRATS-PCELA   PIC X(10).
262 10     05 WA-C9-DTRNSF-LP-PCELA      PIC X(10).
272  5     05 WA-C9-CBACEN-DESC-RURAL    PIC S9(9)V USAGE COMP-3.
277  4     05 WA-C9-CCONTR-CORP-PCELA    PIC S9(7)V USAGE COMP-3.
281 10     05 WA-C9-DULT-ADTTO-PCELA     PIC X(10).
291  1     05 WA-C9-CINDCD-MOTVO-ADTTO   PIC X(1).
292  1     05 WA-C9-CINDCD-PEND-EXTER    PIC X(1).
293  1     05 WA-C9-CINDCD-BLOQ-CONS     PIC X(1).
294 10     05 WA-C9-DBLOQ-CONS-CCORR     PIC X(10).
304 10     05 WA-C9-DULT-PRROG-PCELA     PIC X(10).
314 10     05 WA-C9-DULT-REATV-PCELA     PIC X(10).
324 26     05 WA-C9-HULT-ATULZ           PIC X(26).
350  5     05 WA-C9-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
355  8     05 WA-C9-CTERM                PIC X(8).
363 10     05 WA-C9-DINCL-REG            PIC X(10).
373 09     05 WA-C9-VJURO-REMUN-VENCD    PIC S9(15)V9(2) USAGE COMP-3.
382 09     05 WA-C9-VMORA-VENCD-DESC     PIC S9(15)V9(2) USAGE COMP-3.
391 09     05 WA-C9-VMULTA-VENCD-DESC    PIC S9(15)V9(2) USAGE COMP-3.
400 25     05 FILLER                     PIC X(25).
      ***                                                        ***
      ***  DADOS COMPLEMENTARES                                  ***
      ***                                                        ***
425  2     05 WA-C9-CSIT-DESC-ANTER      PIC S9(3)V USAGE COMP-3.
427  2     05 WA-C9-COD-BAIXA            PIC 9(3)         COMP-3.
429  5     05 WA-C9-CCONTR-CONVE-DESC    PIC S9(9)V USAGE COMP-3.
434  2     05 WA-C9-CVRSAO-CONTR-CONVE   PIC S9(3)V USAGE COMP-3.
436  2     05 WA-C9-CELMTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
438  3     05 WA-C9-DANO-BASE            PIC S9(4)V USAGE COMP-3.
441  3     05 WA-C9-CDEPDC               PIC S9(5)V USAGE COMP-3.
444  5     05 WA-C9-CSEQ-STUDO           PIC S9(9)V USAGE COMP-3.
449  5     05 WA-C9-CCART-CONE           PIC X(5).
454  5     05 WA-C9-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
459  3     05 WA-C9-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
462  2     05 WA-C9-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
464  8     05 WA-C9-VR-BAIXA-PARC        PIC S9(13)V9(2) USAGE COMP-3.
472  1     05 WA-C9-PGTO-TOTAL-PARCIAL   PIC X(01).
473  1     05 WA-C9-CINDCD-CALC-JURO     PIC 9(01).
      ***  TIPO: 1 - CALCULO LINEAR                              ***
      ***        2 - CALCULO EXPONENCIAL                         ***
474 10     05 WA-C9-DREFT-MOVTO-DIA      PIC X(10).
484 2      05 WA-C9-CBCO-CHEQ-MOD        PIC S9(3)V      USAGE COMP-3.
486 3      05 WA-C9-CAG-CHEQ-MOD         PIC S9(5)V      USAGE COMP-3.
489 6      05 WA-C9-CCTA-CHEQ-MOD        PIC S9(11)V     USAGE COMP-3.
495 4      05 WA-C9-CCHEQ-MOD            PIC S9(7)V      USAGE COMP-3.
499 8      05 WA-C9-VCHEQC-MOD           PIC S9(13)V9(2) USAGE COMP-3.
507 46     05 FILLER                     PIC  X(46).
      ***  DADOS DO SACADO TITULO                                ***
553 5      05 WA-F1-CCNPJ-CPF            PIC S9(9)         COMP-3.
558 3      05 WA-F1-CFLIAL-CNPJ          PIC S9(5)         COMP-3.
561 2      05 WA-F1-CCTRL-CNPJ-CPF       PIC S9(2)         COMP-3.
563 60     05 WA-F1-IPSSOA-SACDO         PIC  X(60).
623 3      05 WA-F1-CCEP                 PIC S9(5)         COMP-3.
626 2      05 WA-F1-CCEP-COMPL           PIC S9(3)         COMP-3.
      ***  DADOS DA OPERACAO                                     ***
628 2      05 WA-C5-CBCO                 PIC S9(3)         COMP-3.
630 3      05 WA-C5-CAG-BCRIA            PIC S9(5)         COMP-3.
633 7      05 WA-C5-CCTA-BCRIA-CLI       PIC S9(13)        COMP-3.
640 2      05 WA-C5-CBCO-OPER-OPER       PIC S9(3)         COMP-3.
642 3      05 WA-C5-CAG-OPER-OPER        PIC S9(5)         COMP-3.
645 2      05 WA-C5-CPRODT               PIC S9(3)         COMP-3.
647 2      05 WA-C5-CSPROD-DESC-COML     PIC S9(3)         COMP-3.
649 2      05 WA-C5-CMEIO-ENTRD-DESC     PIC S9(3)         COMP-3.
651 1      05 WA-C5-CINDCD-TX-ESPCL      PIC  9.
652 10     05 WA-C5-DINIC-OPER-DESC      PIC  X(10).
662 2      05 WA-C5-CTPO-DESC-COML       PIC S9(3)         COMP-3.
664 37     05 FILLER                     PIC  X(37).
