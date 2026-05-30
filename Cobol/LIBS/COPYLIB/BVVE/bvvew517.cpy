      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW517                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3517             *
      * DATA      : 09/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01053 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW517-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW517-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      *                                2 - DO HISTORICO                *
      * BVVEW517-FLUXO-MSGEM       :   FLUXO DA MENSAGEM               *
      *                                'E' - ENVIADO                   *
      *                                'R' - RECEBIDO                  *
      * BVVEW517-E-CMNTR-MSGRA     :   CHAVE DA MENSAGEM               *
      * BVVEW517-E-CCONTR          :   CONTRATO BRADESCO               *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW517-REGISTRO.
               10 BVVEW517-BLOCO-ENTRADA.
                   15 BVVEW517-E-CHAMADOR          PIC  X(008).
                   15 BVVEW517-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW517-FLUXO-MSGEM         PIC  X(001).
                   15 BVVEW517-E-CMNTR-MSGRA       PIC  X(023).
HSM                15 BVVEW517-E-DATA-CONSULTA     PIC  X(010).
               10 BVVEW517-BLOCO-RESTART.
                   15 BVVEW517-CHAVE-INI           PIC  9(004).
                   15 BVVEW517-CHAVE-FIM           PIC  9(004).
               10 BVVEW517-BLOCO-PERSISTENCIA.
                   15 BVVEW517-AREA                PIC  X(1000).
               10 BVVEW517-BLOCO-RETORNO.
                   15 BVVEW517-COD-RET             PIC  9(002).
      *
               10 BVVEW517-ENTRADA-W513.
                   15 BVVEW517-E-CHAMADOR-W513     PIC  X(008).
                   15 BVVEW517-E-OPCAO-PESQ-W513   PIC  9(001).
                   15 BVVEW517-E-DT-MOV-INI-W513   PIC  X(010).
                   15 BVVEW517-E-DT-MOV-FIM-W513   PIC  X(010).
                   15 BVVEW517-E-HR-MOV-INI-W513   PIC  X(008).
                   15 BVVEW517-E-HR-MOV-FIM-W513   PIC  X(008).
                   15 BVVEW517-E-ORIGEM-W513.
                       20 BVVEW517-E-CBCO-ORIGE-W513 PIC  9(003).
                       20 BVVEW517-E-ISPB-ORIGE-W513 PIC  X(008).
                   15 BVVEW517-E-DESTINO.
                       20 BVVEW517-E-CBCO-DSTNO-W513    PIC  9(003).
                       20 BVVEW517-E-ISPB-DSTNO-W513    PIC  X(008).
                   15 BVVEW517-E-EVENTO-W513.
                       20 BVVEW517-E-CSIST-EXTER-W513   PIC  X(003).
                       20 BVVEW517-E-CEVNTO-MSG-W513    PIC  9(004).
                       20 BVVEW517-E-CFLUXO-MSG-W513    PIC  X(002).
                   15 BVVEW517-E-NU-CTRL-EMIS-W513      PIC  X(023).
                   15 BVVEW517-E-NU-CTRL-IF-W513        PIC  X(020).
BR0821             15 BVVEW517-E-CCONTR                 PIC  X(020).
                   15 BVVEW517-E-SITU-MSGEM-W513        PIC  9(003).
                   15 BVVEW517-E-FLUXO-MSGEM-W513       PIC  X(001).
                   15 BVVEW517-E-EIBTRMID-W513          PIC  X(008).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
