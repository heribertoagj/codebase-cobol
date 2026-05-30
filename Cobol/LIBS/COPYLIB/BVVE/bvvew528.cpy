      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW528                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3528             *
      * DATA      : 09/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01062 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW528-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW528-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      *                                2 - DO HISTORICO                *
      * BVVEW528-FLUXO-MSGEM       :   FLUXO DA MENSAGEM               *
      *                                'E' - ENVIADO                   *
      *                                'R' - RECEBIDO                  *
      * BVVEW528-E-CMNTR-MSGRA     :   CHAVE DA MENSAGEM               *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW528-REGISTRO.
               10 BVVEW528-BLOCO-ENTRADA.
                   15 BVVEW528-E-CHAMADOR          PIC  X(008).
                   15 BVVEW528-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW528-FLUXO-MSGEM         PIC  X(001).
                   15 BVVEW528-E-CMNTR-MSGRA       PIC  X(023).
                   15 BVVEW528-E-EIBTRMID          PIC  X(008).
                   15 BVVEW528-E-FASE              PIC  9(001).
HSM                15 BVVEW528-E-DATA-CONSULTA     PIC  X(010).
               10 BVVEW528-BLOCO-RESTART.
                   15 BVVEW528-CHAVE-INI           PIC  9(004).
                   15 BVVEW528-CHAVE-FIM           PIC  9(004).
               10 BVVEW528-BLOCO-RETORNO.
                   15 BVVEW528-COD-RET             PIC  9(002).
               10 BVVEW528-BLOCO-PERSISTENCIA.
                   15 BVVEW528-AREA                PIC  X(1000).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
