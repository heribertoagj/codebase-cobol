      *-------------------------------------------------------------*
      * I#RUEC20                                                    *
      * AREA DE COMUNICACAO COM O MODULO RUEC7718 DE VALIDACAO DO   *
      *                              CADASTRO RURAL                 *
      *-------------------------------------------------------------*
       01  R20-AREA-R7718.
           03 R20-ENVIO-R7718.
              05 R20-PGMCHMD-R7718        PIC  X(08).
              05 R20-CCLUB-R7718          PIC  9(11).
              05 FILLER                   PIC  X(20).
           03 R20-RETORNO-R7718.
              05 R20-RET-CODE-R7718       PIC  9(02).
      *
      *            01 - DADOS RECEBIDOS INCONSISTENTES
      *            02 - ALTERACAO SEM ERROS
      *            03 - ALTER. C/ ERROS GRAVADOS NA TABELA DE APOIO
      *            04 - CADASTRO COM CSIT = V COM ERROS
      *            05 - CADASTRO COM CSIT = V SEM ERROS
      *            06 - CLUB INEXISTENTE NO RUECB063
      *            99 - PROBLEMAS DB2
              05 R20-CTPO-PROTR-R7718     PIC  9(02).
              05 R20-SQLCA-R7718          PIC  X(136).
              05 R20-ERRO-AREA-R7718      PIC  X(107).
              05 FILLER                   PIC  X(20).
