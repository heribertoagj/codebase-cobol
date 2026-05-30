      *I#DCOMND
      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMND *
      * CRIACAO   : 02/2007                                   *
      * DESCRICAO : INTERFACE PFEB X DCOM - EXPURGAR AGENDA-  *
      *             MENTOS EFETIVAMENTE.                      *
      *             AGENDAMENTOS ARMAZENADOS NAS BASES HISTO- *
      *             RICAS DO PFEB, QUE DO PONTO DE VISTA DO   *
      *             DCOM PODEM SER EXPURGADOS.                *
      * TAMANHO   : LRECL  040                                *
      **********************************************************
       01      DCOMND-EXP-AGENDS.
         05      DCOMND-EXPURGAR-AGENDAMENTOS.
           10    DCOMND-CIDTFD-SEQ-PGTO      PIC  X(16).
           10    DCOMND-CORIGE-PGTO          PIC S9(01)V COMP-3.
           10    DCOMND-CCGC-CPF             PIC S9(09)V COMP-3.
           10    DCOMND-CFLIAL-CGC           PIC S9(05)V COMP-3.
           10    DCOMND-CCTRL-CGC            PIC S9(02)V COMP-3.
         05      DCOMND-NRO-OPER-DESCONTO    PIC  9(13)  COMP-3.
         05      FILLER                      PIC  X(06).
      *
