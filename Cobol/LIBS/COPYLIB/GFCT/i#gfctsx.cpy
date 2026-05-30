      *****************************************************************
      * NOME DA INC - I#GFCTSX                                        *
      * DESCRICAO   - ARQUIVO ADESCOMP                                *
      * DATA        - MAIO/2006                                       *
      * RESPONSAVEL - MARCELO CREMM - PROCWORK - 30 POS.              *
      *****************************************************************
      *
       01  REG-ADESCOMP.
           03 GFCTSX-AGENCIA                PIC  9(005)    COMP-3.
           03 GFCTSX-CONTA                  PIC  9(007)    COMP-3.
           03 GFCTSX-PAB                    PIC  9(005)    COMP-3.
           03 GFCTSX-PACOTE                 PIC  9(005)    COMP-3.
           03 GFCTSX-DINI                   PIC  9(009)    COMP-3.
           03 GFCTSX-DFIM                   PIC  9(009)    COMP-3.
           03 GFCTSX-AGPTO                  PIC  9(003)    COMP-3.
BI0412     03 GFCTSX-VISUALIZA              PIC  X(001).
BI0412*       'S' - SIM
BI0412*       'N' - NAO
BI0412     03 FILLER                        PIC  X(004).
