      *===============================================================*
      * IDENTIFICACAO...........: BVVEW502                            *
      * DATA DE CRIACAO ........: 08/2020                             *
      * ANALISTA RESPONSAVEL ...: KLEBER SANTOS                       *
      * --------------------------------------------------------------*
      * DESCRICAO: AREA DE COMUNICACAO COM O MODULO BVVE5002          *
      *            ATUALIZA TABELAS BVVEB076 THIST_DIA_CATAO          *
      *                             BVVEB093 TRESUL_MNTRC_DIA_CATAO   *
      *===============================================================*
      * ALTERACOES:                                                   *
      *                                                               *
      * DATA        NOME       DESCRICAO                              *
      * =========== ========== ====================================== *
      *                                                               *
      *===============================================================*
      *
          05 BVVE502-ENTRADA.
      *
             10 BVVE502-TIPO-ATUALIZACAO     PIC 9(02).
                88 INCLUI-LINHA          VALUE 01.
                88 ALTERA-CSNLZD-MSGRA   VALUE 02.
      *
             10 BVVE502-BVVEB076.
                15 BVVE502-NMNTRC-TRFGO-ARQ  PIC S9(018) COMP-3.
                15 BVVE502-CPTCAO-TBELA      PIC S9(002) COMP-3.
                15 BVVE502-NESTRT-INFO-BACEN PIC S9(003) COMP-3.
                15 BVVE502-NESTRT-INFO-BACEN-N PIC X(001).
                15 BVVE502-NVRSAO-ESTRT-INFO PIC S9(003) COMP-3.
                15 BVVE502-NVRSAO-ESTRT-INFO-N PIC X(001).
                15 BVVE502-CERRO-MSGEM-SIST  PIC  X(008).
                15 BVVE502-CERRO-MSGEM-SIST-N PIC X(001).
                15 BVVE502-CIDTFD-ORIGE-ARQ  PIC S9(001) COMP-3.
                15 BVVE502-CIDTFD-ORIGE-ARQ-N PIC X(001).
                15 BVVE502-IARQ-MNTRC        PIC  X(080).
                15 BVVE502-IARQ-MNTRC-N      PIC  X(001).
                15 BVVE502-CCTRL-RQUIS-INSTC PIC  X(020).
                15 BVVE502-CCTRL-RQUIS-INSTC-N PIC X(001).
                15 BVVE502-CCTRL-RQUIS-ORIGN PIC  X(020).
                15 BVVE502-CCTRL-RQUIS-ORIGN-N PIC X(001).
                15 BVVE502-CSIT-RQUIS-ARQ    PIC S9(002) COMP-3.
                15 BVVE502-CSIT-RQUIS-ARQ-N  PIC  X(001).
                15 BVVE502-HPROCM-ARQ        PIC  X(026).
                15 BVVE502-HPROCM-ARQ-N      PIC  X(001).
                15 BVVE502-DMOVTO-INFO-ARQ   PIC  X(010).
                15 BVVE502-DMOVTO-INFO-ARQ-N PIC  X(001).
                15 BVVE502-CIDTFD-SEQ-ARQ    PIC  X(001).
                15 BVVE502-CIDTFD-SEQ-ARQ-N  PIC  X(001).
                15 BVVE502-NTMNHO-ARQ-ORIGN  PIC S9(004)V9(3) COMP-3.
                15 BVVE502-NTMNHO-ARQ-ORIGN-N PIC X(001).
                15 BVVE502-NTMNHO-ARQ-FNAL   PIC S9(004)V9(3) COMP-3.
                15 BVVE502-NTMNHO-ARQ-FNAL-N PIC  X(001).
                15 BVVE502-EARQ-ORIGN        PIC  X(200).
                15 BVVE502-EARQ-ORIGN-N      PIC  X(001).
                15 BVVE502-EARQ-FNAL         PIC  X(200).
                15 BVVE502-EARQ-FNAL-N       PIC  X(001).
                15 BVVE502-EARQ-PROCM        PIC  X(200).
                15 BVVE502-EARQ-PROCM-N      PIC  X(001).
                15 BVVE502-CSNLZD-MSGRA      PIC S9(003) COMP-3.
                15 BVVE502-CSNLZD-MSGRA-N    PIC  X(001).
                15 BVVE502-CPROCS-ENTRG-BACEN PIC X(100).
                15 BVVE502-CPROCS-ENTRG-BACEN-N PIC X(001).
                15 BVVE502-CISPB-EMISR-MSGEM PIC  X(008).
                15 BVVE502-CISPB-EMISR-MSGEM-N PIC X(001).
                15 BVVE502-CISPB-DSTNA-MSGEM PIC  X(008).
                15 BVVE502-CISPB-DSTNA-MSGEM-N PIC X(001).
                15 BVVE502-NMNTRC-TRFGO-ORIGE PIC S9(018).
                15 BVVE502-NMNTRC-TRFGO-ORIGE-N PIC X(001).
      *
             10 BVVE502-BVVEB093.
                15 BVVE502-NRESUL-MNTRC-ARQ  PIC S9(018) COMP-3.
                15 BVVE502-NMNTRC-TRFGO-ARQ  PIC S9(018) COMP-3.
                15 BVVE502-CCTRL-RQUIS-INSTC PIC  X(020).
                15 BVVE502-CCTRL-RQUIS-ORIGN PIC  X(020).
                15 BVVE502-CSNLZD-MSGRA      PIC S9(003) COMP-3.
                15 BVVE502-CISPB-EMISR-MSGEM PIC  X(008).
                15 BVVE502-CISPB-DSTNA-MSGEM PIC  X(008).
                15 BVVE502-CFASE-PROCM       PIC S9(004) COMP-3.
                15 BVVE502-CFASE-PROCM-N     PIC  X(001).
                15 BVVE502-QMSGEM-ENVID      PIC S9(010) COMP-3.
                15 BVVE502-QMSGEM-RECBD      PIC S9(010) COMP-3.
                15 BVVE502-DINCL-RESUL-MNTRC PIC  X(010).
                15 BVVE502-HINCL-RESUL-MNTRC PIC  X(008).
                15 BVVE502-NRELTV-PROCM-RESUL PIC S9(010) COMP-3.
                15 BVVE502-CSIST-EXTER-CMARA PIC  X(004).
                15 BVVE502-CEVNTO-EXTER-CMARA PIC X(003).
                15 BVVE502-CFLUXO-EXTER-CMARA PIC X(003).
                15 BVVE502-HINCL-RESUL-BACEN PIC  X(026).
      *
          05 BVVE502-SAIDA.
             10 BVVE502-COD-RETORNO          PIC  9(002).
                88 PROCESSO-OK     VALUE 00.
                88 ERRO-DADOS      VALUE 01.
                88 ERRO-MQ         VALUE 77.
                88 ERRO-DB2        VALUE 88.
             10 BVVE502-MQ-COMPLETION-CODE   PIC  9(008)  COMP.
             10 BVVE502-MQ-REASON-CODE       PIC  9(008)  COMP.
             10 BVVE502-SQLCODE              PIC S9(009).
      ******************************************************************
