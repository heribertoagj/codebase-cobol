      *****************************************************************
      * NOME DA INC - I#GFCTRZ                                        *
      *               ========                                        *
      * DESCRICAO   - ARQUIVO RESULTANTE DE SELECAO DE ADESOES        *
      * DATA        - MAIO/2006                                       *
      * RESPONSAVEL - MARCELO CREMM - PROCWORK - 50 POS.              *
      *                                                               *
      *****************************************************************

       01  GFCTRZ-ADESOES.
           03 GFCTRZ-AGENCIA                PIC  9(005)    COMP-3.
           03 GFCTRZ-RAZAO                  PIC  9(005)    COMP-3.
           03 GFCTRZ-CONTA                  PIC  9(007)    COMP-3.
           03 GFCTRZ-PAB                    PIC  9(005)    COMP-3.
           03 CHAVE.
              05 GFCTRZ-PACOTE              PIC  9(005)    COMP-3.
              05 GFCTRZ-AGPTO               PIC  9(003)    COMP-3.
              05 GFCTRZ-SEQ                 PIC  9(009)    COMP-3.
              05 GFCTRZ-DATA                PIC  X(010).
           03 GFCTRZ-DINI                   PIC  9(009)    COMP-3.
           03 GFCTRZ-DFIM                   PIC  9(009)    COMP-3.
BI0412     03 FILLER                        PIC  X(002).
BI0412     03 GFCTRZ-VISUALIZA              PIC  X(001).
BI0412*       'S' - SIM
BI0412*       'N' - NAO
BI0412     03 FILLER                        PIC  X(004).
