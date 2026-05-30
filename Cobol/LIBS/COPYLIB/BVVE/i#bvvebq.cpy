      *----------------------------------------------------------------*
      *  I#BVVEBQ - AREA DE COMUNICACAO MODULO BVVE0573                *
      *  OBTER DESCRICAO DA BANDEIRA BCPP                              *
      *  TAMANHO: 400                                                  *
      *----------------------------------------------------------------*

       01  BVVEBQ-COMU-AREA.
           03 BVVEBQ-ENTRADA.
              05 BVVEBQ-VISAO              PIC 9(003).
              05 BVVEBQ-BANDEIRA           PIC 9(003).
           03 BVVEBQ-SAIDA.
              05 BVVEBQ-COD-RETORNO        PIC 9(002).
              05 BVVEBQ-DESC-BANDEIRA      PIC X(060).
           03 BVVEBQ-ERRO.
              05 BVVEBQ-AREA-7100          PIC X(107).
              05 BVVEBQ-AREA-SQLCA         PIC X(136).
           03 FILLER                       PIC X(089).
