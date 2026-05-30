      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMUT *
      * DESCRICAO : INFORMACOES DE ENVIO A BAIXA              *
      *                                                       *
      *                                                       *
      * TAMANHO   : LRECL - 0000                              *
      *                                                       *
      *********************************************************
      *                                                                *
      ******************************************************************
      ******************************************************************
       01  UT-REGISTRO.
           10 UT-NSOLTC-ENCRR-CTA   PIC S9(17)V USAGE COMP-3.
           10 UT-DANO-OPER-DESC     PIC  9(004) VALUE ZEROS.
           10 UT-NSEQ-OPER-DESC     PIC  9(009) VALUE ZEROS.
           10 UT-VBAIXA-TOTAL       PIC  9(015)V99 VALUE ZEROS.
           10 UT-CAG-DEB-BCRIA      PIC 9(005) VALUE ZEROS.
           10 UT-CCTA-DEB-BCRIA-CLI PIC 9(013) VALUE ZEROS.
           10 UT-SALDO-VENCER       PIC 9(015)V99  VALUE ZEROS.
           10 UT-SALDO-VENCIDO      PIC 9(015)V99   VALUE ZEROS.
           10 UT-QTDE-PARC-VENCER   PIC 9(004) VALUE ZEROS.
           10 UT-QTDE-PARC-VENCIDAS PIC 9(004) VALUE ZEROS.
           10 UT-DATA-ACAO-BAIXA    PIC X(10).
           10 UT-SOLICITACAO-BAIXA  PIC X(01).
           10 FILLER                PIC X(50).
