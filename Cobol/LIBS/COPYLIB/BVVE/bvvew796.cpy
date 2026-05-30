      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW796                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3796             *
      * DATA      : 11/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM INFORMATICA                                  *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01104 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW796-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW796-E-NMNTRC-TRFGO-ARQ:   CHAVE DA MENSAGEM               *
      * BVVEW796-E-NETAPA-MNTRC-TRFGO: CHAVE DA MENSAGEM               *
      * BVVEW796-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      * BVVEW796-E-NU-CTRL-EMS     :   NUMERO DE CONTROLE EMISSOR      *
      * BVVEW796-E-EVNTO           :   EVENTO(CODIGO DA MENSAGEM       *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW796-REGISTRO.
               10 BVVEW796-BLOCO-ENTRADA.
                   15 BVVEW796-E-CHAMADOR           PIC  X(008).
                   15 BVVEW796-E-NMNTRC-TRFGO-ARQ   PIC  9(018).
                   15 BVVEW796-E-NETAPA-MNTRC-TRFGO PIC  9(002).
                   15 BVVEW796-E-OPCAO-PESQ         PIC  9(001).
                   15 BVVEW796-E-NU-CTRL-EMS        PIC  X(023).
                   15 BVVEW796-E-ARQUIVO            PIC  X(050).
               10 BVVEW796-BLOCO-PERSISTENCIA.
                   15 BVVEW796-AREA                 PIC  X(1000).
               10 BVVEW796-BLOCO-RETORNO.
                   15 BVVEW796-COD-RET              PIC  9(002).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
