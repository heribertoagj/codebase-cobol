      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. RCOR5050.
       AUTHOR.     7COMM.
      *================================================================*
      *                 P R I M E   I N F O R M A T I C A              *
      *----------------------------------------------------------------*
      *    PROGRAMA....: RCOR5050                                      *
      *    PROGRAMADOR.: 7COMM                                         *
      *    ANALISTA....: 7COMM                                         *
      *    DATA........: SET/2025                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....: BUSCA CONTRATOS COM ERRO                      *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#RCOR31 - AREA DE COMUNICACAO                              *
      *    I#BRAD7C - AREA TRATAMENTO DE ERROS                         *
      *----------------------------------------------------------------*
      *    TABELAS.....:                                               *
      *    TMNFB004 - DB2PRD.TERRO_MSGEM_SIST                          *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       WORKING-STORAGE                 SECTION.
      *================================================================*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*  INICIO DA WORKING RCOR5050  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*   AREA INDEXADORES           *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(05) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)          VALUE
           '*   AREA AUXILIARES            *'.
      *----------------------------------------------------------------*

       77  WRK-FIM-CURSOR              PIC  X(01)          VALUE SPACES.
       01  WRK-TIMESTAMP-INI           PIC X(26)         VALUE
                                           '2999-12-31-23.59.59.999999'.

       01  WRK-TIMESTAMP-FIM           PIC X(26)         VALUE
                                           '2999-12-31-23.59.59.999999'.

       01  IND-CHAR                    PIC  9(05) COMP-3  VALUE ZEROS.
       01  IND-CHAR2                   PIC  9(05) COMP-3  VALUE ZEROS.

       01  WRK-TIMESTAMP.
           05  WRK-ANO                 PIC  X(004)       VALUE  SPACES.
           05  FILLER                  PIC  X(001)       VALUE  '-'.
           05  WRK-MES                 PIC  X(002)       VALUE  SPACES.
           05  FILLER                  PIC  X(001)       VALUE  '-'.
           05  WRK-DIA                 PIC  X(002)       VALUE  SPACES.
           05  FILLER                  PIC  X(001)       VALUE  '-'.
           05  WRK-HORA                PIC  X(002)       VALUE  SPACES.
           05  FILLER                  PIC  X(001)       VALUE  '.'.
           05  WRK-MNT                 PIC  X(002)       VALUE  SPACES.
           05  FILLER                  PIC  X(001)       VALUE  '.'.
           05  WRK-SGD                 PIC  X(002)       VALUE  SPACES.
           05  FILLER                  PIC  X(001)       VALUE  '.'.
           05  WRK-MLS                 PIC  X(006)       VALUE  SPACES.

       01  WRK-WMSGEM-PROCS-CREDT.
           49 WRK-WMSGEM-PROCS-CREDT-LEN
              PIC S9(4) USAGE COMP VALUE ZEROS.
           49 WRK-WMSGEM-PROCS-CREDT-TEXT
              PIC X(32500)         VALUE SPACES.

       01  WRK-APOIO.
           05  FILLER                  PIC X(045)          VALUE SPACES.
           05  WRK-REF                 PIC X(011)          VALUE ZEROS.

       01  WRK-APOIO2.
           05  FILLER                  PIC X(014)          VALUE SPACES.
           05  WRK-VALOR               PIC X(019)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*   AREA PARA DB2              *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RCORB002
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*   AREA DECLARACAO CURSOR     *'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE  CSR01-RCORB002    CURSOR FOR
                SELECT  HINCL_REG          ,
                        CMSGEM_PROCS_CREDT ,
                        WMSGEM_PROCS_CREDT ,
                        CCTRL_MSGEM_CREDT
                  FROM  DB2PRD.TWORKF_MSGEM_CREDT
                 WHERE  HINCL_REG        >= :WRK-TIMESTAMP-INI
                   AND  HINCL_REG        <= :WRK-TIMESTAMP-FIM
                  ORDER  BY  HINCL_REG     ASC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#RCOR31'.
       COPY 'I#BRAD7C'.
       01  LNK-SQLCA                   PIC X(136).

      *================================================================*
       PROCEDURE                       DIVISION USING RCOR31-REGISTRO
                                                      ERRO-AREA
                                                      LNK-SQLCA.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-PROCEDIMENTOS-INICIAIS.

           PERFORM 3000-BUSCA-LISTA-ERROS.

           PERFORM 5000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCEDIMENTOS-INICIAIS     SECTION.
      *----------------------------------------------------------------*

           IF (RCOR31-E-TPCONS         NOT EQUAL 1 AND 2)
               MOVE 10                 TO RCOR31-S-COD-RETORNO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RCOR5050'         TO ERR-PGM
               MOVE 'DADOS DE ENTRADA INVALIDO - RCOR10-CCUSTO'
                                       TO ERR-TEXTO
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       3000-BUSCA-LISTA-ERROS          SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1.

           PERFORM 3100-ABRIR-CSR01-TMNFB004.

           PERFORM 3200-LER-CSR01-TMNFB004.

           PERFORM 3300-MONTAR-OCORRENCIAS UNTIL
                   WRK-FIM-CURSOR      EQUAL 'S'.

           COMPUTE IND-1 = IND-1 - 1
           MOVE IND-1                  TO RCOR31-S-TOT-ERROS.

           PERFORM 3400-FECHAR-CSR01-TMNFB004.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-ABRIR-CSR01-TMNFB004       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR.
           MOVE RCOR31-E-DIA-INI       TO WRK-DIA
           MOVE RCOR31-E-MES-INI       TO WRK-MES
           MOVE RCOR31-E-ANO-INI       TO WRK-ANO
           MOVE '00'                   TO WRK-HORA
           MOVE '01'                   TO WRK-MNT
           MOVE '01'                   TO WRK-SGD
           MOVE '000001'               TO WRK-MLS
           MOVE WRK-TIMESTAMP          TO WRK-TIMESTAMP-INI

           MOVE RCOR31-E-DIA-FIM       TO WRK-DIA
           MOVE RCOR31-E-MES-FIM       TO WRK-MES
           MOVE RCOR31-E-ANO-FIM       TO WRK-ANO
           MOVE '23'                   TO WRK-HORA
           MOVE '59'                   TO WRK-MNT
           MOVE '59'                   TO WRK-SGD
           MOVE '999999'               TO WRK-MLS
           MOVE WRK-TIMESTAMP          TO WRK-TIMESTAMP-FIM

           EXEC SQL
               OPEN CSR01-RCORB002
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 99                 TO RCOR31-S-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TWORKF_MSGEM_CREDT'
                                       TO ERR-DBD-TAB
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE 'RCOR5050'         TO ERR-PGM
               MOVE SQLCA(01:136)      TO LNK-SQLCA
               PERFORM 5000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-LER-CSR01-TMNFB004         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR01-RCORB002
                 INTO  :RCORB002.HINCL-REG          ,
                       :RCORB002.CMSGEM-PROCS-CREDT ,
                       :RCORB002.WMSGEM-PROCS-CREDT ,
                       :RCORB002.CCTRL-MSGEM-CREDT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 99                 TO RCOR31-S-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TERRO_MSGEM_SIST' TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE 'RCOR5050'         TO ERR-PGM
               MOVE SQLCA(01:136)      TO LNK-SQLCA
               PERFORM 5000-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-MONTAR-OCORRENCIAS         SECTION.
      *----------------------------------------------------------------*


           EVALUATE  CMSGEM-PROCS-CREDT OF RCORB002

           WHEN  'COR0005R1'
           WHEN  'COR0005E'
                  PERFORM  4200-OBTER-MSG-COR0001E-5E
           END-EVALUATE.

           PERFORM 3200-LER-CSR01-TMNFB004.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3400-FECHAR-CSR01-TMNFB004      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-RCORB002
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 99                 TO RCOR31-S-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TERRO_MSGEM_SIST' TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0040'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE 'RCOR5050'         TO ERR-PGM
               MOVE SQLCA(01:136)      TO LNK-SQLCA
               PERFORM 5000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4200-OBTER-MSG-COR0001E-5E      SECTION.
      *----------------------------------------------------------------*

           PERFORM
           VARYING  IND-CHAR         FROM  1 BY 1
             UNTIL  IND-CHAR      GREATER  32493
             IF  WMSGEM-PROCS-CREDT    OF RCORB002(IND-CHAR:4)
                                       EQUAL 'ECOR'
                 PERFORM 4250-SELECT-RCORB002
                 IF  SQLCODE                 EQUAL  +100
                     CONTINUE
                 ELSE
                 MOVE  WMSGEM-PROCS-CREDT
                                       OF RCORB002(IND-CHAR:8)
                                       TO RCOR31-S-CODERRO(IND-1)
                 PERFORM
                 VARYING  IND-CHAR2         FROM  1 BY 1
                 UNTIL  IND-CHAR2      GREATER  32493
                   IF  WRK-WMSGEM-PROCS-CREDT(IND-CHAR2:6)
                                               EQUAL '{1100}'
                       MOVE WRK-WMSGEM-PROCS-CREDT(IND-CHAR2:33)
                                       TO WRK-APOIO2
                       IF WRK-VALOR    EQUAL '*******************'
                           MOVE ZEROS  TO RCOR31-S-VALOR(IND-1)
                       ELSE
                           MOVE WRK-VALOR   TO RCOR31-S-VALOR(IND-1)
                       END-IF
                   END-IF
                 END-PERFORM
                 MOVE  HINCL-REG       OF RCORB002(1:10)
                                       TO RCOR31-S-DATA(IND-1)
                 ADD 1                 TO IND-1
               END-IF
             END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       4200-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4250-SELECT-RCORB002            SECTION.
      *----------------------------------------------------------------*

           MOVE 'COR0005  '            TO CMSGEM-PROCS-CREDT
                                       OF RCORB002

           EXEC SQL
               SELECT
                   WMSGEM_PROCS_CREDT
               INTO
                   :WRK-WMSGEM-PROCS-CREDT
               FROM  DB2PRD.TWORKF_MSGEM_CREDT
               WHERE CCTRL_MSGEM_CREDT   = :RCORB002.CCTRL-MSGEM-CREDT
                 AND CMSGEM_PROCS_CREDT  = :RCORB002.CMSGEM-PROCS-CREDT
               ORDER BY HINCL_REG DESC
               FETCH FIRST 1 ROWS ONLY
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TWORKF_MSGEM_CREDT'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0001'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 5000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL  +100
               CONTINUE
           ELSE
               MOVE WRK-WMSGEM-PROCS-CREDT(1:56)
                                       TO WRK-APOIO
               MOVE WRK-REF            TO RCOR31-S-REF(IND-1)
           END-IF.

      *----------------------------------------------------------------*
       4250-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
