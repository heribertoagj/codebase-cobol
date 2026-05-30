      *****************************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMNM         *
      * DESCRICAO : MOVIMENTO DE APROPRIACAO - EVOLUCAO               *
      * TAMANHO   : LRECL = 224                                       *
      *****************************************************************

       01  DCOMNM-AREA.
           05  DCOMNM-DADOS-PARCELA.
               10  DCOMNM-ANO-OPER          PIC  9(004).
               10  DCOMNM-SEQ-OPER          PIC  9(009).
               10  DCOMNM-NUM-PARC          PIC  9(005).
               10  DCOMNM-BCO-CED           PIC  9(003).
               10  DCOMNM-AGE-CED           PIC  9(005).
               10  DCOMNM-SIT-DESC          PIC  9(003).
               10  DCOMNM-VCTO-PARC         PIC  X(008).
               10  DCOMNM-VLR-PRINC         PIC  9(015)V99    COMP-3.
               10  DCOMNM-TXA-JRS-MES       PIC  9(003)V99999 COMP-3.
               10  DCOMNM-VLR-JRS-PARC      PIC  9(015)V99 COMP-3.
               10  DCOMNM-VLR-JRS-DIA-AVD   PIC  9(015)V99 COMP-3.
               10  DCOMNM-VLR-JRS-ACD-AVDD  PIC  9(015)V99 COMP-3.
               10  DCOMNM-VLR-JRS-ACT-AVMC  PIC  9(015)V99 COMP-3.
               10  DCOMNM-VLR-JRS-ACG-AVTC  PIC  9(015)V99 COMP-3.
               10  DCOMNM-VLR-PRES          PIC  9(015)V99 COMP-3.
               10  DCOMNM-DTA-ULTAPR        PIC  X(008).
               10  DCOMNM-VLR-JRS-MORA-LEG  PIC  9(015)V99 COMP-3.
               10  DCOMNM-ALQ-IOF-VENC      PIC  9(003)V99999 COMP-3.
               10  DCOMNM-VLR-IOF-VENC      PIC  9(015)V99    COMP-3.
               10  DCOMNM-VLR-JRS-MORA-GER  PIC  9(015)V99    COMP-3.
               10  DCOMNM-VLR-SDO-DEVPRINC  PIC  9(015)V99    COMP-3.
               10  DCOMNM-DTA-TRF-CRATS     PIC  X(008).
               10  DCOMNM-VLR-JRS-TRF-CRATS PIC  9(015)V99    COMP-3.
               10  DCOMNM-DTA-TRANSF-LP     PIC  X(008).
               10  DCOMNM-CONTCOR           PIC  9(007).
               10  DCOMNM-PRODUTO           PIC  9(003).
               10  DCOMNM-SUBPROT           PIC  9(003).
               10  DCOMNM-FATOGER           PIC  9(003).
               10  DCOMNM-PESSOA-FJA        PIC  9(001).
               10  DCOMNM-MEIOENT           PIC  9(003).
               10  DCOMNM-CBCO-CHEQ-MOD     PIC  9(003)       COMP-3.
               10  DCOMNM-CAG-CHEQ-MOD      PIC  9(005)       COMP-3.
               10  DCOMNM-CCTA-CHEQ-MOD     PIC  9(011)       COMP-3.
               10  DCOMNM-CCHEQ-MOD         PIC  9(007)       COMP-3.
               10  FILLER                   PIC  X(010).
