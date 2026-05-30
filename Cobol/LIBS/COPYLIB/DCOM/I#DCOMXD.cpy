      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMXD *
      * CRIACAO   : 01/2007                                   *
      * DESCRICAO : VALIDA CONSULTAS CONE PARA OPERACAO E     *
      *             CONTRATO LIMITE                           *
      *           - ELEMENTO 06 = OPERACAO                    *
      *             ELEMENTO 09 = CONVENIO                    *
      *             ELEMENTO 10 = CONTRATO LIMITE             *
      *                                                       *
      * APLICACAO : MODULO DCOM6436                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************
      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************
       01  ROTENT-6436-AREA.
           03 ROTENT-6436-ENVIO.
              05 ROTENT-6436-COD-ENV        PIC  9(04).
      *          INFORMAR ZEROS
              05 ROTENT-6436-MSG-ENV        PIC  X(79).
      *          INFORMAR BRANCOS
              05 ROTENT-6436-REST-ENV       PIC  9(05).
      *          INFORMAR NUMERO DE REGISTROS PARA RESTART
              05 ROTENT-6436-FLAG-ENV       PIC  X(01).
      *          INFORMAR N
              05 ROTENT-6436-START-ENV      PIC  X(01).
      *          INFORMAR N NA PRIMEIRA CHAMADA QUANDO NAO TIVER RESTART
      *          INFORMAR S NA PRIMEIRA CHAMADA QUANDO TIVER RESTART
              05 ROTENT-6436-CFUNC-ENV      PIC  9(09).
      *          INFORMAR CODIGO DO FUNCIONARIO
              05 ROTENT-6436-CTERM-ENV      PIC  X(08).
      *          INFORMAR TERMINAL
              05 ROTENT-6436-ELEM-ENV       PIC  X(02).
              05 ROTENT-6436-VALOR-ENV      PIC  9(13)V99.
              05 ROTENT-6436-VLR-REST-ENV   PIC  9(13)V99.
              05 ROTENT-6436-QTD-OCOR-ENV   PIC  9(02).
              05 FILLER       OCCURS      50  TIMES.
                 07 ROTENT-6436-AGCONE-ENV  PIC  X(04).
                 07 ROTENT-6436-ANOCONE-ENV PIC  X(04).
                 07 ROTENT-6436-SEQCONE-ENV PIC  X(09).
                 07 ROTENT-6436-CART-ENV    PIC  X(04).
                 07 ROTENT-6436-VLR-LIM-ENV PIC  9(13)V99.
                 07 ROTENT-6436-VLR-UTL-ENV PIC  9(13)V99.
                 07 ROTENT-6436-VLR-DIS-ENV PIC  9(13)V99.
                 07 ROTENT-6436-CGAR-ENV    PIC  X(04).
                 07 ROTENT-6436-SIT-ENV     PIC  X(10).
      *********************************************************
      * AREA DE RETORNO                                       *
      *********************************************************
           03 ROTSAI-6436-RETORNO.
              05 ROTSAI-6436-COD-RET       PIC  9(04).
              05 ROTSAI-6436-MENS-RET      PIC  X(79).
              05 ROTSAI-6436-REST-RET      PIC  9(13)V99.
              05 ROTSAI-6436-FLAG-RET      PIC  X(01).
              05 ROTSAI-OCOR.
                 07 ROTSAI-6436-AGCONE-RET  PIC  X(04).
                 07 ROTSAI-6436-ANOCONE-RET PIC  X(04).
                 07 ROTSAI-6436-SEQCONE-RET PIC  X(09).
                 07 ROTSAI-6436-CART-RET    PIC  X(04).
                 07 ROTSAI-6436-VLR-LIM-RET PIC  9(13)V99.
                 07 ROTSAI-6436-VLR-UTL-RET PIC  9(13)V99.
                 07 ROTSAI-6436-VLR-DIS-RET PIC  9(13)V99.
                 07 ROTSAI-6436-CGAR-RET    PIC  X(04).
                 07 ROTSAI-6436-SIT-RET     PIC  X(10).
