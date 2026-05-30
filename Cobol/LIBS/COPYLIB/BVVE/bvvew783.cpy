      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW783                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3783             *
      * DATA      : 11/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM INFORMATICA                                  *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 1192 BYTES                                         *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW783-E-CMNTRC-ARQUIVO  :   CHAVE DO ARQUIVO                *
      * BVVEW783-E-CMNTRC-MSGRA    :   CHAVE DA MENSAGEM               *
      * BVVEW783-E-NCTRL-IF        :   NUMERO DE CONTROLE DA INSTI-    *
      *                                TUICAO FINANCEIRA               *
      * BVVEW783-E-NOME-ARQUIVO    :   NOME DO ARQUIVO                 *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW783-REGISTRO.
               10 BVVEW783-BLOCO-ENTRADA.
                   15 BVVEW783-E-CMNTRC-ARQUIVO    PIC  9(018).
                   15 BVVEW783-E-CMNTRC-MSGRA      PIC  X(023).
                   15 BVVEW783-E-NCTRL-IF          PIC  X(020).
                   15 BVVEW783-E-NOME-ARQUIVO      PIC  X(050).
                   15 BVVEW783-E-TP-CONSULTA       PIC  X(001).
               10 BVVEW783-BLOCO-PERSISTENCIA.
                   15 BVVEW783-AREA                PIC  X(1000).
               10 BVVEW783-BLOCO-RETORNO.
                   15 BVVEW783-COD-RET             PIC  9(002).
                   15 BVVEW783-MSG-RET             PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
