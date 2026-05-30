      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW795                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3795             *
      * DATA      : 10/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM INFORMATICA                                  *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01338 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW795-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW795-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      *                                2 - DO HISTORICO                *
      * BVVEW795-E-NMNTRC-TRFGO-ARQ:   CHAVE DA MENSAGEM               *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW795-REGISTRO.
               10 BVVEW795-BLOCO-ENTRADA.
                   15 BVVEW795-E-CHAMADOR          PIC  X(008).
                   15 BVVEW795-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW795-E-NMNTRC-TRFGO-ARQ  PIC  9(018).
               10 BVVEW795-BLOCO-CONTROLE.
                   15 BVVEW795-NMNTRC-TRFGO-ARQ    PIC  9(018)
                                                   OCCURS 8 TIMES.
                   15 BVVEW795-NETAPA-MNTRC-TRFGO  PIC  9(002)
                                                   OCCURS 8 TIMES.
               10 BVVEW795-BLOCO-RESTART.
                   15 BVVEW795-HPROCM-REST-INI     PIC  X(026).
                   15 BVVEW795-HINIC-REST-INI      PIC  X(026).
                   15 BVVEW795-HPROCM-REST-FIM     PIC  X(026).
                   15 BVVEW795-HINIC-REST-FIM      PIC  X(026).
               10 BVVEW795-BLOCO-PERSISTENCIA.
                   15 BVVEW795-AREA                PIC  X(1000).
               10 BVVEW795-BLOCO-RETORNO.
                   15 BVVEW795-COD-RET             PIC  9(002).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
