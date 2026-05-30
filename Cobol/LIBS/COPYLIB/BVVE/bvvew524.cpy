      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW524                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3524             *
      * DATA      : 29/10/2014                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 00600 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW524-CHAMADOR          : PROGRAMA CHAMADOR                 *
      * BVVEW524-AREA-CHAMADOR     : COMMAREA DO CHAMADOR              *
      * BVVEW524-MAPA-CHAMADOR     : MAPA DA TELA DO CHAMADOR          *
      *                                                                *
      *********************** DADOS DE SAIDA ***************************
      *                                                                *
      * BVVEW524-OP-SEL            : OPCAO SELECIONADA                 *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW524-REGISTRO.
               10 BVVEW524-BLOCO-ENTRADA.
                   15 BVVEW524-CHAMADOR            PIC  X(008).
                   15 BVVEW524-CFUNC-BDSCO         PIC  X(007).
                   15 BVVEW524-EIBTRMID            PIC  X(008).
                   15 BVVEW524-AREA-CHAMADOR       PIC  X(017).
                   15 BVVEW524-MAPA-CHAMADOR       PIC  X(400).
               10 BVVEW524-BLOCO-RESTART.
                   15 BVVEW524-CSNLZD-REST-INI     PIC  9(003).
                   15 BVVEW524-CSNLZD-REST-FIM     PIC  9(003).
               10 BVVEW524-BLOCO-SAIDA.
                   15 BVVEW524-CSNLZD-MSGRA        PIC  9(003).
                   15 BVVEW524-RSNLZD-MSGRA        PIC  X(100).
               10 FILLER                           PIC  X(051).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      *****************************************************************
