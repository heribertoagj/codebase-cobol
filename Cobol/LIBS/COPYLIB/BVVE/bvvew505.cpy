      *===============================================================*
      * IDENTIFICACAO...........: BVVEW505                            *
      * DATA DE CRIACAO ........: 08/2020                             *
      * ANALISTA RESPONSAVEL ...: KLEBER SANTOS                       *
      * --------------------------------------------------------------*
      * DESCRICAO: AREA DE COMUNICACAO COM O MODULO BVVE5005          *
      *            ATUALIZA VARIAVEIS ACUMULADORAS - CICS             *
      *===============================================================*
      * ALTERACOES:                                                   *
      *                                                               *
      * DATA        NOME       DESCRICAO                              *
      * =========== ========== ====================================== *
      *                                                               *
      *===============================================================*
      *
         03 BVVE505-AREA.
      *
            05 BVVE505-ENTRADA-DADOS.
      *
                10 BVVE505-TIPO-ATUALIZACAO  PIC 9(02).
                   88 INCLUI-LINHA           VALUE 01.
                   88 ALTERA-CSNLZD-MSGRA    VALUE 02.
      *
                10 BVVE505-BVVEB076.
                   15 BVVE505-NMNTRC-TRFGO-ARQ    PIC S9(018) COMP-3.
                   15 BVVE505-CPTCAO-TBELA        PIC S9(002) COMP-3.
                   15 BVVE505-NESTRT-INFO-BACEN   PIC S9(003) COMP-3.
                   15 BVVE505-NESTRT-INFO-BACEN-N PIC X(001).
                   15 BVVE505-NVRSAO-ESTRT-INFO   PIC S9(003) COMP-3.
                   15 BVVE505-NVRSAO-ESTRT-INFO-N PIC X(001).
                   15 BVVE505-CERRO-MSGEM-SIST    PIC X(008).
                   15 BVVE505-CERRO-MSGEM-SIST-N  PIC X(001).
                   15 BVVE505-CIDTFD-ORIGE-ARQ    PIC S9(001) COMP-3.
                   15 BVVE505-CIDTFD-ORIGE-ARQ-N  PIC X(001).
                   15 BVVE505-IARQ-MNTRC          PIC  X(080).
                   15 BVVE505-IARQ-MNTRC-N        PIC  X(001).
                   15 BVVE505-CCTRL-RQUIS-INSTC   PIC X(020).
                   15 BVVE505-CCTRL-RQUIS-INSTC-N PIC X(001).
                   15 BVVE505-CCTRL-RQUIS-ORIGN   PIC X(020).
                   15 BVVE505-CCTRL-RQUIS-ORIGN-N PIC X(001).
                   15 BVVE505-CSIT-RQUIS-ARQ      PIC S9(002) COMP-3.
                   15 BVVE505-CSIT-RQUIS-ARQ-N    PIC X(001).
                   15 BVVE505-HPROCM-ARQ          PIC  X(026).
                   15 BVVE505-HPROCM-ARQ-N        PIC  X(001).
                   15 BVVE505-DMOVTO-INFO-ARQ     PIC X(010).
                   15 BVVE505-DMOVTO-INFO-ARQ-N   PIC X(001).
                   15 BVVE505-CIDTFD-SEQ-ARQ      PIC  X(001).
                   15 BVVE505-CIDTFD-SEQ-ARQ-N    PIC X(001).
                   15 BVVE505-NTMNHO-ARQ-ORIGN PIC S9(004)V9(3) COMP-3.
                   15 BVVE505-NTMNHO-ARQ-ORIGN-N  PIC X(001).
                   15 BVVE505-NTMNHO-ARQ-FNAL  PIC S9(004)V9(3) COMP-3.
                   15 BVVE505-NTMNHO-ARQ-FNAL-N   PIC X(001).
                   15 BVVE505-EARQ-ORIGN          PIC  X(200).
                   15 BVVE505-EARQ-ORIGN-N        PIC  X(001).
                   15 BVVE505-EARQ-FNAL           PIC  X(200).
                   15 BVVE505-EARQ-FNAL-N         PIC  X(001).
                   15 BVVE505-EARQ-PROCM          PIC  X(200).
                   15 BVVE505-EARQ-PROCM-N        PIC  X(001).
                   15 BVVE505-CSNLZD-MSGRA        PIC S9(003) COMP-3.
                   15 BVVE505-CSNLZD-MSGRA-N      PIC  X(001).
                   15 BVVE505-CPROCS-ENTRG-BACEN   PIC X(100).
                   15 BVVE505-CPROCS-ENTRG-BACEN-N PIC X(001).
                   15 BVVE505-CISPB-EMISR-MSGEM    PIC X(008).
                   15 BVVE505-CISPB-EMISR-MSGEM-N  PIC X(001).
                   15 BVVE505-CISPB-DSTNA-MSGEM    PIC X(008).
                   15 BVVE505-CISPB-DSTNA-MSGEM-N  PIC X(001).
                   15 BVVE505-NMNTRC-TRFGO-ORIGE   PIC S9(018).
                   15 BVVE505-NMNTRC-TRFGO-ORIGE-N PIC X(001).
      *
                10 BVVE505-BVVEB093.
                   15 BVVE505-NRESUL-MNTRC-ARQ     PIC S9(018) COMP-3.
                   15 BVVE505-NMNTRC-TRFGO-ARQ     PIC S9(018) COMP-3.
                   15 BVVE505-CCTRL-RQUIS-INSTC    PIC X(020).
                   15 BVVE505-CCTRL-RQUIS-ORIGN    PIC X(020).
                   15 BVVE505-CSNLZD-MSGRA         PIC S9(003) COMP-3.
                   15 BVVE505-CISPB-EMISR-MSGEM    PIC X(008).
                   15 BVVE505-CISPB-DSTNA-MSGEM    PIC X(008).
                   15 BVVE505-CFASE-PROCM          PIC S9(004) COMP-3.
                   15 BVVE505-CFASE-PROCM-N        PIC  X(001).
                   15 BVVE505-QMSGEM-ENVID         PIC S9(010) COMP-3.
                   15 BVVE505-QMSGEM-RECBD         PIC S9(010) COMP-3.
                   15 BVVE505-DINCL-RESUL-MNTRC    PIC X(010).
                   15 BVVE505-HINCL-RESUL-MNTRC    PIC X(008).
                   15 BVVE505-NRELTV-PROCM-RESUL   PIC S9(010) COMP-3.
                   15 BVVE505-CSIST-EXTER-CMARA    PIC X(004).
                   15 BVVE505-CEVNTO-EXTER-CMARA   PIC X(003).
                   15 BVVE505-CFLUXO-EXTER-CMARA   PIC X(003).
                   15 BVVE505-HINCL-RESUL-BACEN    PIC X(026).
      *
            05 BVVE505-SAIDA.
              10 BVVE505-INDICADOR-VARIAVEL PIC 9(02).
                 88 TMNF-OK-ENV-M               VALUE 01.
                 88 TMNF-OK-REC-M               VALUE 02.
                 88 TMNF-NOK-ENV-M              VALUE 03.
                 88 TMNF-NOK-ENV-M              VALUE 04.
                 88 TMNF-OK-ENV-A               VALUE 05.
                 88 TMNF-OK-REC-A               VALUE 06.
                 88 TMNF-NOK-ENV-A              VALUE 07.
                 88 TMNF-NOK-REC-A              VALUE 08.
              10 BVVE505-COD-RETORNO       PIC  9(002).
                 88 PROCESSO-OK  VALUE 00.
                 88 ERRO-DADOS   VALUE 01.
                 88 ERRO-DB2     VALUE 88.
              10 BVVE505-SQLCODE           PIC S9(009).
