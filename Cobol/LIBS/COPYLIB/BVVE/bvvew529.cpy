      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW529                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3529             *
      * DATA      : 09/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 04623 BYTES                                        *
      *********************** DADOS DE ENTRADA *************************
      * BVVEW529-CHAMADOR          : PROGRAMA CHAMADOR                 *
      * BVVEW529-CERRO             : CODIGO DO ERRO                    *
      * BVVEW529-DT-PROCM          : CODIGO DO ERRO                    *
      *                                                                *
      *********************** DADOS DE RETORNO *************************
      * BVVEW529-COD-RET           : CODIGO DE RETORNO                 *
      *                             00 - SUCESSO                       *
      *                             08 - ERRO                          *
      * BVVEW529-MSG-RET           : MENSAGEM RETORNADA EM CASO DE ERRO*
      *                                                                *
      *********************** DADOS DE PERSISTENCIA ********************
      * BVVEW529-MAPA              : MAPA DA TELA DO CHAMADOR          *
      * BVVEW529-AREA              : COMMAREA DO CHAMADOR              *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW529-REGISTRO.
               10 BVVEW529-BLOCO-ENTRADA.
                   15 BVVEW529-CHAMADOR            PIC  X(008).
                   15 BVVEW529-CTRANS-PROG         PIC  X(004).
                   15 BVVEW529-CTERM-USUAR         PIC  X(015).
HSM                15 BVVEW529-E-DATA-CONSULTA     PIC  X(010).
               10 BVVEW529-BLOCO-RESTART.
                   15 BVVEW529-RST-NCTRL           PIC  9(005).
               10 BVVEW529-BLOCO-RETORNO.
                   15 BVVEW529-COD-RET             PIC  9(002).
                   15 BVVEW529-MSG-RET             PIC  X(079).
               10 BVVEW529-BLOCO-PERSISTENCIA.
                   15 BVVEW529-MAPA                PIC  X(1500).
                   15 BVVEW529-AREA                PIC  X(3000).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      *****************************************************************
