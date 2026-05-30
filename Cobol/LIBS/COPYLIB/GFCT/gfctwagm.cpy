      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *================================================================*
      * NOME DA INC - GFCTWAGM                                         *
      * DESCRICAO: COPIA DA INCLUDE I#GFCTRZ + TIPO PESSOA(ADESAO/CLIE)*
      * DATA        - ABRIL/2009                                       *
      * RESPONSAVEL - VINICIUS C. MADUREIRA - SONDA PROCWORK           *
      * LRECL       - 050                                              *
      ******************************************************************
      *
       01  WAGM-ADESOES.
           03 WAGM-AGENCIA                PIC  9(005)    COMP-3.
           03 WAGM-RAZAO                  PIC  9(005)    COMP-3.
           03 WAGM-CONTA                  PIC  9(007)    COMP-3.
           03 WAGM-PAB                    PIC  9(005)    COMP-3.
           03 WAGM-CHAVE.
              05 WAGM-PACOTE              PIC  9(005)    COMP-3.
              05 WAGM-AGPTO               PIC  9(003)    COMP-3.
              05 WAGM-SEQ                 PIC  9(009)    COMP-3.
              05 WAGM-DATA                PIC  X(010).
           03 WAGM-DINI                   PIC  9(009)    COMP-3.
           03 WAGM-DFIM                   PIC  9(009)    COMP-3.
BI0810     03 WAGM-PESSOA-ADESAO          PIC  X(001).
BI0810*       '1' - FISICA
BI0810*       '2' - JURIDICA
BI0810*       '3' - AMBAS
BI0810     03 WAGM-PESSOA-CLIE            PIC  X(001).
BI0810*       'F' - FISICA
BI0810*       'J' - JURIDICA
BI0412     03 WAGM-VISUALIZA              PIC  X(001).
BI0412*       'S' - SIM
BI0412*       'N' - NAO
BI0412     03 FILLER                      PIC  X(004).

