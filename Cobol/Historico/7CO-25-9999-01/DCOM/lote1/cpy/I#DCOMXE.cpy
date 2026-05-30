      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMXE *
      * CRIACAO   : 01/2007                                   *
      * DESCRICAO : VALIDA PERFIL DO USUARIO                  *
      *                                                       *
      * APLICACAO : MODULO DCOM6437                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

       01  ROTENT-6437-ENVIO.
           05 ROTENT-6437-COD-RETORNO    PIC  X(04).
           05 ROTENT-6437-MSG-RETORNO    PIC  X(79).
           05 ROTENT-6437-RESTART        PIC  9(05).
           05 ROTENT-6437-FLAG           PIC  X(01).
           05 ROTENT-6437-CFUNC-ENV      PIC  9(09).
      *       INFORMAR CODIGO DO FUNCIONARIO-      -
           05 ROTENT-6437-CTERM          PIC  X(08).
           05 ROTENT-6437-AGEUSU-ENV     PIC  9(05).
      *       INFORMAR AGENCIA DO USUARIO      -
           05 ROTENT-6437-PROD-ENV       PIC  9(03).
      *       INFORMAR O PRODUTO      -
           05 ROTENT-6437-AGEPESQ-ENV    PIC  9(05).
      *       INFORMAR AGENCIA A SER PESQUISADA      -
           05 ROTENT-6437-FUNCAO-ENV     PIC  X(01).
      *       INFORMAR A FUNCAO (I=INCLUSAO, A=ALTERACAO, E=EXCLUSAO
           05 ROTENT-6437-SUBPRO-ENV     PIC  9(03).
      *       INFORMAR O SUBPRODUTO OU ZEROS

      *********************************************************
      * AREA DE RETORNO                                       *
      *********************************************************

       01  ROTSAI-6437-RETORNO.
           05 ROTSAI-6437-COD-RET       PIC  9(04).
           05 ROTSAI-6437-MENS-RET      PIC  X(79).
           05 ROTSAI-6437-RESTART       PIC  9(05).
           05 ROTSAI-6437-FLAG          PIC  X(01).
           05 ROTSAI-6437-RESUL-RET     PIC  9(02).
      *          01-USUARIO DA AGENCIA
      *          02-USUARIO GESTOR
      *          03-USUARIO SEGMENTO
      *          04-USUARIO AGENCIA DIFERENTE
      *          05-USUARIO NAO AUTORIZADO
           05 ROTSAI-6437-CONSISTENCIA.
             10 ROTSAI-6437-TAMANHO-ERRO   PIC  9(01).
             10 ROTSAI-6437-TABELA-ERROS OCCURS 5 TIMES.
                15 ROTSAI-6437-CAMPOS-ERRO PIC  9(01).

