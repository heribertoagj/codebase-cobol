      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW785                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3785             *
      * DATA      : 16/08/2016                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 00600 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW785-CHAMADOR          : PROGRAMA CHAMADOR                 *
      * BVVEW785-AREA-CHAMADOR     : COMMAREA DO CHAMADOR              *
      * BVVEW785-MAPA-CHAMADOR     : MAPA DA TELA DO CHAMADOR          *
      *                                                                *
      *********************** DADOS DE SAIDA ***************************
      *                                                                *
      * BVVEW785-OP-SEL            : OPCAO SELECIONADA                 *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW785-REGISTRO.
               10 BVVEW785-BLOCO-ENTRADA.
                   15 BVVEW785-CHAMADOR            PIC  X(008).
                   15 BVVEW785-CFUNC-BDSCO         PIC  X(007).
                   15 BVVEW785-EIBTRMID            PIC  X(008).
                   15 BVVEW785-AREA-CHAMADOR       PIC  X(017).
                   15 BVVEW785-MAPA-CHAMADOR       PIC  X(400).
               10 BVVEW785-BLOCO-RESTART.
                   15 BVVEW785-CSNLZD-REST-INI     PIC  9(003).
                   15 BVVEW785-CSNLZD-REST-FIM     PIC  9(003).
               10 BVVEW785-BLOCO-SAIDA.
                   15 BVVEW785-CSNLZD-MSGRA        PIC  9(003).
                   15 BVVEW785-RSNLZD-MSGRA        PIC  X(100).
               10 FILLER                           PIC  X(051).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      *****************************************************************
