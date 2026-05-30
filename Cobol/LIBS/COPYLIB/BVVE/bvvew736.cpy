      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW736                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3736             *
      * DATA      : 10/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM INFORMATICA                                  *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01101 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW736-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      *                                2 - DO HISTORICO                *
      * BVVEW736-E-FLUXO-ARQUIVO   :   FLUXO DO ARQUIVO                *
      *                                'E' - ENVIADO                   *
      *                                'R' - RECEBIDO                  *
      * BVVEW736-E-CMNTRC-ARQUIVO  :   CHAVE DO ARQUIVO                *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW736-REGISTRO.
               10 BVVEW736-BLOCO-ENTRADA.
                   15 BVVEW736-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW736-E-FLUXO-ARQUIVO     PIC  X(001).
                   15 BVVEW736-E-CMNTRC-ARQUIVO    PIC  9(018).
               10 BVVEW736-BLOCO-PERSISTENCIA.
                   15 BVVEW736-AREA                PIC  X(1000).
               10 BVVEW736-BLOCO-RETORNO.
                   15 BVVEW736-COD-RET             PIC  9(002).
                   15 BVVEW736-MSG-RET             PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
