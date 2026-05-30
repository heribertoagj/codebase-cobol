      *----------------------------------------------------------------*
      * I#RUEC22                                                       *
      * AREA DE COMUNICACAO COM O  MODULO  RUEC7722 P/ SELECAO DE      *
      *         IMOVEIS CADASTRADAS CADU E RUEC - CAD. RURAL (I/A/E/C) *
      *----------------------------------------------------------------*
       01  R22-AREA-R7722.
           03 R22-ENVIO-R7722.
              05 R22-PGMCHMD-R7722        PIC  X(08).
              05 R22-FUNCAO-R7722         PIC  X(01).
      *             "I" - INCLUSAO CADASTRO RURAL
      *             "A" - ALTERACAO CADASTRO RURAL
      *             "E" - EXCLUSAO CADASTRO RURAL
      *             "C" - CONSULTA CADASTRO RURAL
      *             "P" - PESQUISA P/ EMISSAO DE CEDULAS
              05 R22-QPESQ-R7722          PIC  9(02).
              05 R22-QDESPR-R7722         PIC  9(05).
              05 R22-CCLUB-R7722          PIC  9(10).
              05 R22-TPSSOA-ENV-R7722     PIC  X(01).
              05 R22-SEQPATRM-ENV-R7722   PIC  9(05).
              05 FILLER                   PIC  X(50).
           03 R22-RETORNO-R7722.
              05 R22-RET-CODE-R7722       PIC  9(02).
      *
      *            01 - DADOS INCONSISTENTES
      *            02 - NENHUM REGISTRO ENCONTRADO
      *            03 - REGISTRO ENCONTRADO - ACABOU PESQUISA
      *            04 - REGISTRO ENCONTRADO - PESQUISA CONTINUA
      *            05 - PROBLEMAS CADU - NAO EMITIR CEDULA
      *            99 - PROBLEMAS DB2
      *
              05 R22-MENS-R7722           PIC  X(79).
              05 R22-QRETRN-R7722         PIC  9(02).
              05 R22-INEXCADU-R7722       PIC  X(01).
      *            'S'- INDICA QUE EXISTE IMOVEL DO RUEC EXCL. CADU
      *
              05 FILLER       OCCURS      50  TIMES.
                 07 R22-ESPEC-R7722       PIC  9(03).
                 07 R22-RESPC-R7722       PIC  X(15).
PW1577           07 R22-RTPDOCTO-R7722    PIC  X(40).
                 07 R22-CMUNIC-R7722      PIC  9(09).
                 07 R22-RMUNIC-R7722      PIC  X(40).
                 07 R22-UF-R7722          PIC  X(02).
                 07 R22-DREG-R7722        PIC  X(10).
                 07 R22-CRI-R7722         PIC  X(10).
                 07 R22-CMATRIC-R7722     PIC  X(10).
                 07 R22-CLIVRO-R7722      PIC  X(04).
                 07 R22-CFOLHA-R7722      PIC  X(07).
PW1577           07 R22-MAREA-R7722       PIC  9(06)V9(04).
                 07 R22-VATUAL-R7722      PIC  9(13)V9(02).
                 07 R22-SQPATR-CADU-R7722 PIC  9(05).
                 07 R22-SQPATR-RUEC-R7722 PIC  9(05).
                 07 R22-SQIMOV-R7722      PIC  9(03).
                 07 R22-ORIG-R7722        PIC  X(01).
                 07 R22-RIMOV-R7722       PIC  X(25).
                 07 R22-CIMPNH-R7722      PIC  X(01).
                 07 R22-CINDMIGR-R7722    PIC  X(01).
                 07 R22-CINDEXPDO-R7722   PIC  X(01).
                 07 R22-CTPOESCRT-R7722   PIC  X(01).
                 07 R22-CSITIMOV-R7722    PIC  9(01).
PW1577           07 R22-MAREACULTR-R7722  PIC  9(07)V9999.
PW1577           07 R22-MAREARBNHO-R7722  PIC  9(07)V9999.
PW1577           07 R22-MAREAOUTR-R7722   PIC  9(07)V9999.
              05 FILLER                   PIC  X(50).
              05 R22-SQLCA-R7722          PIC  X(136).
              05 R22-ERRO-AREA-R7722      PIC  X(107).
