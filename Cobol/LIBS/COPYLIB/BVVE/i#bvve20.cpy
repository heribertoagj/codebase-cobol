      *****************************************************************
      *   I#BVVE20 -  AREA DE COMUNICACAO  P/ OS MODULOS :            *
      *                                         INCLUSAO ,            *
      *                                         ALTERACAO,            *
      *                                         EXCLUSAO              *
      *---------------------------------------------------------------*
      *   TAMANHO =    2048 BYTES                                     *
      *****************************************************************
      *
       01  BVVE20-AREA-TRANSACAO.
           03  BVVE20-AREA-DADOS               PIC X(2048).
           03  BVVE20-AREA-RETORNO  REDEFINES  BVVE20-AREA-DADOS.
               05  BVVE20-COD-RETORNO          PIC 9(02).
               05  BVVE20-COD-SQLCODE          PIC 9(05).
               05  BVVE20-MENSAGEM             PIC X(255).
               05  FILLER                      PIC X(1786).
