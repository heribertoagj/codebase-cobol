      *-------------------------------------------------------------*
      * I#RUEC21                                                    *
      * AREA DE COMUNICACAO COM O MODULO RUEC7720 DE VERIFICACAO    *
      *                 DE DADOS RUEC E CADU PARA CORROMPER CEDULA  *
      *-------------------------------------------------------------*
       01  R21-AREA-R7720.
           03 R21-ENVIO-R7720.
              05 R21-PGMCHMD-R7720        PIC  X(08).
              05 R21-FUNCAO-R7720         PIC  X(01).

      *         "C" - VERIFICAR SE CORROMPE CEDULA
      *         "P" - PESQUISAR QTDD/INDCD P/ CONTROLE POST.CEDULAS

              05 R21-CCLUB-R7720          PIC  9(11).
              05 R21-DCEDULA-R7720        PIC  X(26).
              05 R21-CIND-TRES-ENV-R7720  PIC  X(01).
              05 R21-CIND-TCOM-ENV-R7720  PIC  X(01).
              05 R21-CIND-ERES-ENV-R7720  PIC  X(01).
              05 R21-CIND-ECOM-ENV-R7720  PIC  X(01).
              05 R21-CIND-QPFS-ENV-R7720  PIC  X(01).
              05 R21-QPATR-IMOV-ENV-R7720 PIC  9(03).
              05 R21-QPATR-VEIC-ENV-R7720 PIC  9(03).
              05 R21-QLSNG-ENV-R7720      PIC  9(03).
              05 R21-QSEGUR-ENV-R7720     PIC  9(03).
              05 R21-QQDR-SCIAL-ENV-R7720 PIC  9(03).
              05 FILLER                   PIC  X(20).
           03 R21-RETORNO-R7720.
              05 R21-RET-CODE-R7720       PIC  9(02).
      *
      *            00 - CCLUB COM DADOS OK NO RUEC E CADU
      *            01 - DADOS INCONSISTENTES
      *            02 - CCLUB INEXISTENTE NO CADU
      *            03 - CCLUB INEXISTENTE NO RUEC
      *            04 - ALTER. EFET. POST. CEDULA - CORROMPER CEDULA
      *            05 - INFORM.EXCL. POST. CEDULA - CORROMPER CEDULA
      *            99 - PROBLEMAS DB2
      *
              05 R21-CIND-TRES-RET-R7720  PIC  X(01).
              05 R21-CIND-TCOM-RET-R7720  PIC  X(01).
              05 R21-CIND-ERES-RET-R7720  PIC  X(01).
              05 R21-CIND-ECOM-RET-R7720  PIC  X(01).
              05 R21-CIND-QPFS-RET-R7720  PIC  X(01).
              05 R21-QPATR-IMOV-RET-R7720 PIC  9(03).
              05 R21-QPATR-VEIC-RET-R7720 PIC  9(03).
              05 R21-QLSNG-RET-R7720      PIC  9(03).
              05 R21-QSEGUR-RET-R7720     PIC  9(03).
              05 R21-QQDR-SCIAL-RET-R7720 PIC  9(03).
              05 R21-DCAD-PSSOA-R7720     PIC  X(10).
              05 FILLER                   PIC  X(20).
              05 R21-SQLCA-R7720          PIC  X(136).
              05 R21-ERRO-AREA-R7720      PIC  X(107).
