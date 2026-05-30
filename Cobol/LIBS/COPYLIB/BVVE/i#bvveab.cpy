      *==============================================================*
      *   BOOK -  I#BVVEAB                                           *
      *           ARQUIVO DE ENVIO A ROTINA GART - DIARIAMENTE       *
      ****************************************************************
      *   TAMANHO        0080  BYTES                                 *
      ****************************************************************
      *    1 -   REGISTRO HEADER                                     *
      *    2 -   REGISTRO DETALHE                                    *
      ****************************************************************
      *--------------------------------------------------------------*
      *        HEADER  = 1                                           *
      *--------------------------------------------------------------*
       01  REG-AB-HEADER.
           03  AB-HEADER.
               05  ABH-PARTE-001.
                   10  ABH-TP-REGISTRO             PIC 9(001).
                   10  ABH-DT-REFERENCIA           PIC 9(008).
                   10  ABH-CENTRO-CUSTO            PIC X(004).
                   10  ABH-FILLER                  PIC X(067).
      *----------------------------------------------------------------*
      *        DETALHE = 2                                             *
      *----------------------------------------------------------------*
           03  AB-DETALHE                 REDEFINES AB-HEADER.
               05  ABD-DADOS-CONTA-CREDITO.
                   10  ABD-TP-REGISTRO             PIC 9(001).
                   10  ABD-BANCO-DEPOSITO          PIC 9(003)    COMP-3.
                   10  ABD-AGENCIA-DEPOSITO        PIC 9(005)    COMP-3.
                   10  ABD-CONTA-DEPOSITO          PIC 9(013)    COMP-3.
                   10  ABD-RAZAO-DEPOSITO          PIC 9(003)    COMP-3.
                   10  ABD-SUBRAZAO-DEPOSITO       PIC 9(003)    COMP-3.
               05  ABD-DADOS-CEDENTE.
                   10  ABD-BANCO-CEDENTE           PIC 9(003)    COMP-3.
                   10  ABD-AGENCIA-CEDENTE         PIC 9(005)    COMP-3.
                   10  ABD-CONTA-CEDENTE           PIC 9(013)    COMP-3.
                   10  ABD-RAZAO-CEDENTE           PIC 9(003)    COMP-3.
                   10  ABD-SUBRAZAO-CEDENTE        PIC 9(003)    COMP-3.
                   10  ABD-PRODUTO                 PIC 9(005)    COMP-3.
                   10  ABD-DT-VENCIMENTO           PIC 9(008)    COMP-3.
                   10  ABD-VLR-DOCUMENTO           PIC 9(015)V99 COMP-3.
               05  ABD-DADOS-TITULOS.
                   10  ABD-FILLER-1                PIC X(030).
               05  ABD-DADOS-CHEQUES     REDEFINES ABD-DADOS-TITULOS.
                   10  ABD-FILLER-2                PIC X(030).
               05  ABD-DADOS-CARTAO-CREDITO
                                         REDEFINES ABD-DADOS-TITULOS.
                   10  ABD-AGENCIA-CARTAO          PIC 9(005)    COMP-3.
                   10  ABD-CONTA-CARTAO            PIC 9(013)    COMP-3.
                   10  ABD-OPERADORA-CARTAO        PIC 9(003)    COMP-3.
                   10  ABD-FILLER-3                PIC X(018).
