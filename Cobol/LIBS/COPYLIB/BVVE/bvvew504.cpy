      *===============================================================*
      * IDENTIFICACAO...........: BVVEW504                            *
      * DATA DE CRIACAO ........: SET/2020                            *
      * ANALISTA RESPONSAVEL ...: ROSANGELA MAURO                     *
      * --------------------------------------------------------------*
      * DESCRICAO: AREA DE COMUNICACAO COM O MODULO BVVE5004          *
      *            ATUALIZA TABELA BVVEB010-TRESUL_MNTRC_MSGRA        *
      *===============================================================*
      * ALTERACOES:                                                   *
      *                                                               *
      * DATA        NOME       DESCRICAO                              *
      * =========== ========== ====================================== *
      *                                                               *
      *===============================================================*
      *
       05 BVVE504-ENTRADA.
          10 BVVE504-TIPO-ATUALIZACAO         PIC 9(02).
             88 INCLUI-LINHA             VALUE 01.
             88 EXCLUI-LINHA             VALUE 02.
             88 ALTERA-CSNLZD-MSGRA      VALUE 03.
             88 ALTERA-FASE              VALUE 04.
             88 INCLUI-PROBL             VALUE 05.
             88 EXCLUI-PROBL             VALUE 06.
          10 BVVE504-BVVEB010.
             15 BVVE504-NRESUL-MNTRC-MSGRA    PIC S9(018) COMP-3.
             15 BVVE504-CMNTRC-MSGRA          PIC  X(023).
             15 BVVE504-CSNLZD-MSGRA          PIC S9(003) COMP-3.
             15 BVVE504-CISPB-DSTNA-MSGEM     PIC  X(008).
             15 BVVE504-CISPB-EMISR-MSGEM     PIC  X(008).
             15 BVVE504-CCTRL-RQUIS-INSTC     PIC  X(020).
             15 BVVE504-CUNIC-TRFGO-MSGEM     PIC  X(023).
             15 BVVE504-CFASE-PROCM           PIC S9(004) COMP-3.
             15 BVVE504-CFASE-PROCM-N         PIC  X(001).
             15 BVVE504-CSIST-EXTER           PIC  X(003).
             15 BVVE504-CEVNTO-MSGEM          PIC  X(004).
             15 BVVE504-CFLUXO-MSGEM          PIC  X(002).
             15 BVVE504-QMSGEM-RECBD          PIC S9(010) COMP-3.
             15 BVVE504-QMSGEM-ENVID          PIC S9(010) COMP-3.
             15 BVVE504-DINCL-RESUL-MNTRC     PIC  X(010).
             15 BVVE504-HINCL-RESUL-MNTRC     PIC  X(008).
             15 BVVE504-NRELTV-PROCM-RESUL    PIC S9(015) COMP-3.
             15 BVVE504-HINCL-RESUL-BACEN     PIC  X(026).
             15 BVVE504-CIDTFD-ORIGE-ARQ      PIC S9(001) COMP-3.
             15 BVVE504-PGM-CHAMADOR          PIC  X(008).
             15 BVVE504-LOCAL-CHAMADA         PIC  X(004).
      *
