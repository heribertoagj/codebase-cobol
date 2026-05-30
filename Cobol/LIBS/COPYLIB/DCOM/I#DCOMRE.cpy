      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRE                                      *
      * TAMANHO : 0550 BYTES                                           *
      * ARQUIVO : CHEQUE MODIFICADO PARCELA (DCOMB095) COM DADOS       *
      *           DO ARQUIVO RETORNO DO CONTA CORRENTE                 *
      ******************************************************************
BSI001*                    ALTERACAO - BSI TECNOLOGIA                  *
BSI001*----------------------------------------------------------------*
BSI001*  ANALISTA BSI:   FERNANDO LUIZ DE SANTI  -  BSI TECNOLOGIA     *
BSI001*  DATA........:   28/04/2017                                    *
BSI001*----------------------------------------------------------------*
BSI001*  OBJETIVO....:   INCLUIR CAMPOS DE ENCARGOS MORATORIOS.        *
BSI001******************************************************************
       01  RE-REGISTRO.
01  03     05 RE-DANO-OPER-DESC       PIC S9(4)V      USAGE COMP-3.
04  05     05 RE-NSEQ-OPER-DESC       PIC S9(9)V      USAGE COMP-3.
09  03     05 RE-NPCELA-DESC-COML     PIC S9(5)V      USAGE COMP-3.
12  02     05 RE-CBCO                 PIC S9(3)V      USAGE COMP-3.
14  03     05 RE-CAG-BCRIA            PIC S9(5)V      USAGE COMP-3.
17  06     05 RE-CCTA-CORR-BCRIA      PIC S9(11)V     USAGE COMP-3.
23  04     05 RE-CCHEQ                PIC S9(7)V      USAGE COMP-3.
27  26     05 RE-HULT-ATULZ           PIC X(26).
53  03     05 RE-CDEPDC               PIC S9(5)V      USAGE COMP-3.
56  07     05 RE-CCTA-BCRIA-CLI       PIC S9(13)V     USAGE COMP-3.
63  03     05 RE-CFLIAL-EMPR-CHEQC    PIC S9(5)V      USAGE COMP-3.
66  10     05 RE-DVCTO-CHEQ-CTDIA     PIC X(10).
76  08     05 RE-VCHEQC               PIC S9(13)V9(2) USAGE COMP-3.
84  10     05 RE-DMODIF-CHEQC-PCELA   PIC X(10).
94 252     05 RE-RMOTVO-DEVLC-CHEQC.
94  02        49 RMOTVO-DEVLC-CHEQC-LEN  PIC S9(4)       USAGE COMP.
96 250        49 RMOTVO-DEVLC-CHEQC-TEXT PIC X(250).
346 01     05 RE-CINDCD-REAP-CHEQC    PIC S9(1)V      USAGE COMP-3.
347 10     05 RE-DREAP-CHEQC          PIC X(10).
357 01     05 RE-CINDCD-MODLD-PROCM   PIC X(1).
358 01     05 RE-CINDCD-MOTVO-MODIF   PIC X(1).
359 09     05 RE-VSDO-DVDOR-PRINC     PIC S9(15)V9(2) USAGE COMP-3.
368 09     05 RE-VJURO-MORA-LEGAL     PIC S9(15)V9(2) USAGE COMP-3.
377 09     05 RE-VJURO-MORA-GERC      PIC S9(15)V9(2) USAGE COMP-3.
386 09     05 RE-VIOF-PCELA-VENCD     PIC S9(15)V9(2) USAGE COMP-3.
395 09     05 RE-VCUSTO-PCELA-VENCD   PIC S9(15)V9(2) USAGE COMP-3.
404 09     05 RE-VJURO-TRNSF-CRATS    PIC S9(15)V9(2) USAGE COMP-3.
413 09     05 RE-VCUSTO-TRNSF-CRATS   PIC S9(15)V9(2) USAGE COMP-3.
422 04     05 RE-CCONTR-CORP-CHEQC    PIC S9(7)V      USAGE COMP-3.
426 10     05 RE-DVNCLO-REAP-CHEQC    PIC X(10).
436 09     05 RE-VJURO-REMUN-VENCD    PIC S9(15)V9(2) USAGE COMP-3.
445 09     05 RE-VMORA-VENCD-DESC     PIC S9(15)V9(2) USAGE COMP-3.
454 09     05 RE-VMULTA-VENCD-DESC    PIC S9(15)V9(2) USAGE COMP-3.
      **                                                              **
      **----- DADOS DO CONTA CORRENTE                              ---**
      **                                                              **
463 02     05 RE-COD-BAIXA            PIC  9(3)     COMP-3.
           05 RE-VALOR-ENVIADO-CTA-CORR.
465 08        07  RE-VLR-TOTAL        PIC 9(013)V99 COMP-3.
473 08        07  RE-VLR-PRINCIPAL    PIC 9(013)V99 COMP-3.
481 08        07  RE-VLR-ENCARGOS     PIC 9(013)V99 COMP-3.
489 08        07  RE-VLR-IOF          PIC 9(013)V99 COMP-3.
           05 RE-VALOR-DEBITADO-CTA-CORR.
497 08        07  RE-TOTAL-DEB        PIC  9(13)V99 COMP-3.
505 08        07  RE-PRINCIPAL-DEB    PIC  9(13)V99 COMP-3.
513 08        07  RE-ENCARGOS-DEB     PIC  9(13)V99 COMP-3.
521 08        07  RE-IOF-DEB          PIC  9(13)V99 COMP-3.
529 22     05 RE-FILLER               PIC X(22).
