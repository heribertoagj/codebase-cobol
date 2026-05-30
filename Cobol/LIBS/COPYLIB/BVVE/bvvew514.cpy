      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW514                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3514             *
      * DATA      : 19/02/2014                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01116 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW514-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      *                                2 - DO HISTORICO                *
      * BVVEW514-FLUXO-MSGEM       :   FLUXO DA MENSAGEM               *
      *                                'E' - ENVIADO                   *
      *                                'R' - RECEBIDO                  *
      * BVVEW514-E-CMNTR-MSGRA     :   CHAVE DA MENSAGEM               *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW514-REGISTRO.
               10 BVVEW514-BLOCO-ENTRADA.
                   15 BVVEW514-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW514-FLUXO-MSGEM         PIC  X(001).
                   15 BVVEW514-E-CMNTR-MSGRA       PIC  X(023).
HSM                15 BVVEW514-E-DATA-CONSULTA     PIC  X(010).
               10 BVVEW514-BLOCO-PERSISTENCIA.
                   15 BVVEW514-AREA                PIC  X(1000).
               10 BVVEW514-BLOCO-RETORNO.
                   15 BVVEW514-COD-RET             PIC  9(002).
                   15 BVVEW514-MSG-RET             PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
