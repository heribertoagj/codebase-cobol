      *                                                                *
      ******************************************************************
      *                                                                *
      * NOME BOOK : BVVEW015                                           *
      *                                                                *
      * DESCRICAO : AREA DE COMUNICACAO DO MODULO PARA OBTER LISTA DE  *
      *             CREDENCIADORAS E BANDEIRAS                         *
      *                                                                *
      * DATA      : 08/07/2019                                         *
      *                                                                *
      * AUTOR     : REGINALDO PEDROSO DE AQUINO                        *
      *                                                                *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   :  247 BYTES                                         *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVEW015-AREA.
           05 BVVEW015-E-ENTRADA.
              10 BVVEW015-E-NCADTO-AUTRZ-AGNDA       PIC  9(013).
           05 BVVEW015-S-SAIDA.
              10 BVVEW015-S-NCADTO-AUTRZ-AGNDA       PIC  9(013).
              10 BVVEW015-S-COD-RETORNO              PIC  9(005).
              10 BVVEW015-S-MENSAGEM                 PIC  X(079).
      *                                                                *
          03 BVVEW015-A-DADOS-APOIO.
             05 BVVEW015-A-FIM-DADOS             PIC X(001).
             05 BVVEW015-A-SQLCA                 PIC X(136).
      *                                                                *
