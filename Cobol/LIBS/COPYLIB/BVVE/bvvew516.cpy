      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW516                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3516             *
      * DATA      : 09/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01683 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW516-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW516-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      *                                2 - DO HISTORICO                *
      * BVVEW516-E-CMNTR-MSGRA     :   CHAVE DA MENSAGEM               *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW516-REGISTRO.
               10 BVVEW516-BLOCO-ENTRADA.
                   15 BVVEW516-E-CHAMADOR          PIC  X(008).
                   15 BVVEW516-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW516-E-CMNTR-MSGRA       PIC  X(023).
HSM                15 BVVEW516-E-DATA-CONSULTA     PIC  X(010).
.                  15 BVVEW516-BLOCO-WORKFLOW-DDA.
.                     20 BVVEW516-E-CUNIC-TRFGO-MSGEM PIC  X(023).
.                     20 BVVEW516-E-EVENTO.
.                        25 BVVEW516-E-CSIST-EXTER    PIC  X(003).
.                        25 BVVEW516-E-CEVNTO-MSGEM   PIC  X(004).
.                        25 BVVEW516-E-CFLUXO-MSGEM   PIC  X(002).
.                     20 BVVEW516-E-CISPB-EMISR-MSGEM PIC  X(008).
.                     20 BVVEW516-E-CISPB-DSTNA-MSGEM PIC  X(008).
.                     20 BVVEW516-E-HPROCM-MSGEM      PIC  X(026).
.                     20 BVVEW516-E-HCONF-ENVIO-MSGEM  PIC  X(026).
.                     20 BVVEW516-E-HCONF-LEITU-MSGEM  PIC  X(026).
.                     20 BVVEW516-E-RIDTFD-MSGEM      PIC  X(048).
.                     20 BVVEW516-E-CDOMNO-MSGRA-SIST PIC  X(005).
.                     20 BVVEW516-E-CFASE-PROCM       PIC  9(004).
.                     20 BVVEW516-E-RFASE-PROCM       PIC  X(100).
.                     20 BVVEW516-E-HINIC-ETAPA-MNTRC PIC  X(026).
HSM                   20 BVVEW516-E-HFNAL-ETAPA-MNTRC PIC  X(026).
               10 BVVEW516-BLOCO-CONTROLE.
                   15 BVVEW516-CMNTR-MSGRA         PIC  X(023)
                                                   OCCURS 8 TIMES.
                   15 BVVEW516-NETAPA-MNTRC        PIC  9(002)
                                                   OCCURS 8 TIMES.
               10 BVVEW516-BLOCO-RESTART.
                   15 BVVEW516-HINIC-REST-INI      PIC  X(026).
                   15 BVVEW516-HINIC-REST-FIM      PIC  X(026).
               10 BVVEW516-BLOCO-PERSISTENCIA.
                   15 BVVEW516-AREA                PIC  X(1000).
               10 BVVEW516-BLOCO-RETORNO.
                   15 BVVEW516-COD-RET             PIC  9(002).
      *                                                                *
               10 BVVEW516-ENTRADA-W513.
                   15 BVVEW516-E-CHAMADOR-W513     PIC  X(008).
                   15 BVVEW516-E-OPCAO-PESQ-W513   PIC  9(001).
                   15 BVVEW516-E-DT-MOV-INI-W513   PIC  X(010).
                   15 BVVEW516-E-DT-MOV-FIM-W513   PIC  X(010).
                   15 BVVEW516-E-HR-MOV-INI-W513   PIC  X(008).
                   15 BVVEW516-E-HR-MOV-FIM-W513   PIC  X(008).
                   15 BVVEW516-E-ORIGEM-W513.
                       20 BVVEW516-E-CBCO-ORIGE-W513 PIC  9(003).
                       20 BVVEW516-E-ISPB-ORIGE-W513 PIC  X(008).
                   15 BVVEW516-E-DESTINO.
                       20 BVVEW516-E-CBCO-DSTNO-W513    PIC  9(003).
                       20 BVVEW516-E-ISPB-DSTNO-W513    PIC  X(008).
                   15 BVVEW516-E-EVENTO-W513.
                       20 BVVEW516-E-CSIST-EXTER-W513   PIC  X(003).
                       20 BVVEW516-E-CEVNTO-MSG-W513    PIC  9(004).
                       20 BVVEW516-E-CFLUXO-MSG-W513    PIC  X(002).
                   15 BVVEW516-E-NU-CTRL-EMIS-W513      PIC  X(023).
                   15 BVVEW516-E-NU-CTRL-IF-W513        PIC  X(020).
BR0821             15 BVVEW516-E-CCONTR                 PIC  X(020).
                   15 BVVEW516-E-SITU-MSGEM-W513        PIC  9(003).
                   15 BVVEW516-E-FLUXO-MSGEM-W513       PIC  X(001).
                   15 BVVEW516-E-EIBTRMID-W513          PIC  X(008).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
