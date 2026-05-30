      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUECC941.
       AUTHOR.     BSI TECNOLOGIA.
      *================================================================*
      *                    B S I  T E C N O L O G I A                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   RUECC941                                    *
      *    ANALISTA....:   BSI TECNOLOGIA                              *
      *    DATA........:   DEZEMBRO/2015                               *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INDICAR, COM BASE NOS DADOS DA CEDULA,      *
      *                    SE O CADASTRAMENTO DE COORDENADAS GEODESICAS*
      *                    EH OBRIGATORIO.                             *
      *                    LIMPAR O CADASTRO CASO NAO SEJA.            *
      * >>>>>>>>>>>>>>>>>> PROGRAMA CLONE DO RUEC7941 <<<<<<<<<<<<<<<< *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    RUECWGEO - AREA DE COMUNICACAO COM CHAMADORES               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO DB2.                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '***INICIO DA WORKING RUECC941***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*** VARIAVEIS AUXILIARES     ***'.
      *---------------------------------------------------------------*

       01  FILLER.
           10 WRK-MODULO               PIC X(008)          VALUE SPACES.
           10 WRK-FIM-CSR01            PIC X(001)          VALUE SPACES.
           10 WRK-FIM-CSR02            PIC X(001)          VALUE SPACES.

       01  WRK-ACESSO-RURCB096.
           10 WRK-COUNT                PIC S9(09) COMP-3   VALUE ZEROS.

      *---------------------------------------------------------------*
      *  CAMPOS UTILIZADOS PARA O TRATAMENTO DE NULIDADE              *
      *---------------------------------------------------------------*

       01  WRK-B002-NULL.
           05 WRK-B002-CFNALD-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B002-CATVDD-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B002-CORIGE-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B002-CPRODT-NULL      PIC S9(04) COMP VALUE +0.
      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*** AREA DO MODULO RUEC8824  ***'.
      *---------------------------------------------------------------*

       COPY 'RUECW824'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*** COPYBOOK I#BRAD7C        ***'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA AUXILIAR            ***'.
      *----------------------------------------------------------------*

       01  FILLER.
           05 WRK-CJUNC-DEPDC-SZD      PIC +9(05)          VALUE ZEROS.
           05 FILLER REDEFINES WRK-CJUNC-DEPDC-SZD.
              10 FILLER                PIC  X(01).
              10 WRK-CJUNC-DEPDC-ZD    PIC  9(05).

           05 WRK-CCDULA-SZD           PIC +9(09)          VALUE ZEROS.
           05 FILLER REDEFINES WRK-CCDULA-SZD.
              10 FILLER                PIC  X(01).
              10 WRK-CCDULA-ZD         PIC  9(09).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA PARA DB2            ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE SQLCA            ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RUFIB002         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RURCB096         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RURCB096
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RUECV007         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV007
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RUECB09M         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB09M
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RUECB09N         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB09N
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** CURSOR RUECB09M          ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-RUECB09M CURSOR FOR
             SELECT
               CSEQ_MATR_RURAL
             , NSEQ_GLEBA_IMOV
             , NCORND_GEOGR_IMOV
             FROM  DB2PRD.TCORND_GEOGR_CDULA
             WHERE CJUNC_DEPDC         = :RUECB09M.CJUNC-DEPDC
             AND   CCDULA              = :RUECB09M.CCDULA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** CURSOR RUECB09N          ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR02-RUECB09N CURSOR FOR
             SELECT
               CSEQ_MATR_RURAL
              ,NSEQ_GLEBA_IMOV
             , NCORND_GEOGR_IMOV
             FROM  DB2PRD.TCORND_GEOGR_CONTR
             WHERE CCONTR              = :RUECB09N.CCONTR
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(32) VALUE
           '*** FIM DA WORKING RUECC941  ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE                         SECTION.
      *---------------------------------------------------------------*

       COPY 'RUECWGEO'.
       01  LNK-ERRO-AREA               PIC X(107).
       01  LNK-SQLCA                   PIC X(136).

      *================================================================*
       PROCEDURE DIVISION              USING RUECWGEO-COPYBOOK
                                             LNK-ERRO-AREA
                                             LNK-SQLCA.
      *================================================================*

      *---------------------------------------------------------------*
       0000-INICIO                     SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*

           CALL 'CKRS1000'.

           INITIALIZE RUECWGEO-BLOCO-SAIDA.
           MOVE  ZEROS                 TO RUECWGEO-COD-RETORNO.
           MOVE 'N'                    TO RUECWGEO-COORD-OBRIG.
           MOVE 'CONSULTA EFETUADA'    TO RUECWGEO-MENSAGEM.

           PERFORM 1100-CONSISTIR-ENTRADA.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-CONSISTIR-ENTRADA          SECTION.
      *---------------------------------------------------------------*

           IF  RUECWGEO-E-CJUNC-DEPDC  NOT NUMERIC
               MOVE ZEROS              TO RUECWGEO-E-CJUNC-DEPDC
           END-IF.

           IF  RUECWGEO-E-CCDULA       NOT NUMERIC
               MOVE ZEROS              TO RUECWGEO-E-CCDULA
           END-IF.

           IF  RUECWGEO-E-CCONTR       NOT NUMERIC
               MOVE ZEROS              TO RUECWGEO-E-CCONTR
           END-IF.

           IF  RUECWGEO-E-EXCLUIR      NOT EQUAL 'S' AND 'N'
               MOVE 'N'                TO RUECWGEO-E-EXCLUIR
           END-IF.

           IF  RUECWGEO-E-CJUNC-DEPDC  EQUAL ZEROS
           AND RUECWGEO-E-CCDULA       EQUAL ZEROS
           AND RUECWGEO-E-CCONTR       EQUAL ZEROS
               MOVE 'DADOS DE ENTRADA NAO INFORMADOS'
                                       TO RUECWGEO-MENSAGEM
               MOVE 04                 TO RUECWGEO-COD-RETORNO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  (RUECWGEO-E-CJUNC-DEPDC EQUAL ZEROS
           AND  RUECWGEO-E-CCDULA      NOT EQUAL ZEROS )
           OR  (RUECWGEO-E-CJUNC-DEPDC NOT EQUAL ZEROS
           AND  RUECWGEO-E-CCDULA      EQUAL ZEROS     )
               MOVE 'DADOS DA CEDULA INCOMPLETOS'
                                       TO RUECWGEO-MENSAGEM
               MOVE 04                 TO RUECWGEO-COD-RETORNO
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 2001-CONSISTENCIA-LOGICA.

           IF CCTRO-CUSTO              OF RUFIB002 EQUAL 'CRUR'
              PERFORM 2210-OBTER-AGRO
           ELSE
              PERFORM 2200-OBTER-CEDULA
           END-IF.

           PERFORM 2300-OBTER-SUBPROGRAMA.

           PERFORM 2400-ACESSAR-PARAMETRO.

           IF  WRK-COUNT               EQUAL ZEROS
               MOVE 'N'                TO RUECWGEO-COORD-OBRIG
               IF RUECWGEO-E-EXCLUIR   EQUAL 'S'
                  IF CCTRO-CUSTO       OF RUFIB002  EQUAL 'CRUR'
                     PERFORM 2600-EXCLUIR-CORND-CONTR
                  ELSE
                     PERFORM 2500-EXCLUIR-CORND-CEDULA
                     PERFORM 2600-EXCLUIR-CORND-CONTR
                  END-IF
               END-IF
           ELSE
               MOVE 'S'                TO RUECWGEO-COORD-OBRIG
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2001-CONSISTENCIA-LOGICA        SECTION.
      *---------------------------------------------------------------*

           MOVE ZEROS                  TO CJUNC-DEPDC      OF RUECV007
                                          CCDULA           OF RUECV007
                                          CCONTR           OF RUECV007
                                          CJUNC-DEPDC      OF RUFIB002
                                          CCDULA           OF RUFIB002
                                          CCONTR           OF RUFIB002.

           MOVE SPACES                 TO CCTRO-CUSTO      OF RUFIB002

           IF  RUECWGEO-E-CCONTR       EQUAL ZEROS
               MOVE RUECWGEO-E-CJUNC-DEPDC
                                       TO CJUNC-DEPDC        OF RUFIB002
               MOVE RUECWGEO-E-CCDULA  TO CCDULA             OF RUFIB002
           ELSE
               PERFORM 2100-OBTER-CONTRATO
           END-IF

           IF CCTRO-CUSTO              OF RUFIB002 NOT EQUAL 'CRUR'
              MOVE CJUNC-DEPDC         OF RUFIB002
                                       TO WRK-CJUNC-DEPDC-SZD
              MOVE CCDULA              OF RUFIB002
                                       TO WRK-CCDULA-SZD

              IF  RUECWGEO-E-CCDULA       EQUAL ZEROS
                  MOVE WRK-CJUNC-DEPDC-ZD TO RUECWGEO-E-CJUNC-DEPDC
                  MOVE WRK-CCDULA-ZD      TO RUECWGEO-E-CCDULA
              ELSE
                  IF RUECWGEO-E-CJUNC-DEPDC
                                          NOT EQUAL WRK-CJUNC-DEPDC-ZD
                  OR RUECWGEO-E-CCDULA    NOT EQUAL WRK-CCDULA-ZD
                      MOVE 'CEDULA E CONTRATO DIVERGENTES'
                                          TO RUECWGEO-MENSAGEM
                      MOVE 04             TO RUECWGEO-COD-RETORNO
                      PERFORM 3000-FINALIZAR
                  END-IF
              END-IF
           END-IF.

      *---------------------------------------------------------------*
       2001-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2100-OBTER-CONTRATO             SECTION.
      *---------------------------------------------------------------*

           MOVE RUECWGEO-E-CCONTR      TO CCONTR           OF RUFIB002.

           EXEC SQL
             SELECT
               CJUNC_DEPDC
             , CCDULA
             , CCTRO_CUSTO
             INTO
               :RUFIB002.CJUNC-DEPDC
             , :RUFIB002.CCDULA
             , :RUFIB002.CCTRO-CUSTO
             FROM  DB2PRD.CONTRATO_RURAL
             WHERE CCONTR             = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUFIB002'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2100'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 04                 TO RUECWGEO-COD-RETORNO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       2200-OBTER-CEDULA               SECTION.
      *---------------------------------------------------------------*

           MOVE RUECWGEO-E-CJUNC-DEPDC TO CJUNC-DEPDC      OF RUECV007.
           MOVE RUECWGEO-E-CCDULA      TO CCDULA           OF RUECV007.

           EXEC SQL
             SELECT
               VALUE ( CCONTR , 0 )
             , CATVDD_RURAL
             , CFNALD_RURAL
             , CPRODT_RURAL
             , CORIGE_REC
             , CMODLD_RURAL
             , VFINAN
             INTO
               :RUECV007.CCONTR
             , :RUECV007.CATVDD-RURAL
             , :RUECV007.CFNALD-RURAL
             , :RUECV007.CPRODT-RURAL
             , :RUECV007.CORIGE-REC
             , :RUECV007.CMODLD-RURAL
             , :RUECV007.VFINAN
             FROM  DB2PRD.V01CEDULA_TEMPR
             WHERE CJUNC_DEPDC         = :RUECV007.CJUNC-DEPDC
             AND   CCDULA              = :RUECV007.CCDULA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUFIB002'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2200'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'CEDULA NAO ENCONTRADA'
                                       TO RUECWGEO-MENSAGEM
               MOVE 04                 TO RUECWGEO-COD-RETORNO
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2210-OBTER-AGRO                 SECTION.
      *---------------------------------------------------------------*

           INITIALIZE                     WRK-B002-NULL

           MOVE RUECWGEO-E-CCONTR      TO CCONTR           OF RUFIB002

           EXEC SQL
             SELECT
               CATVDD_RURAL
             , CFNALD_RURAL
             , CPRODT_RURAL
             , CORIGE_REC
             , CMODLD_RURAL
             , VFINAN
             INTO
               :RUECV007.CATVDD-RURAL
                   :WRK-B002-CATVDD-NULL
             , :RUECV007.CFNALD-RURAL
                   :WRK-B002-CFNALD-NULL
             , :RUECV007.CPRODT-RURAL
                   :WRK-B002-CPRODT-NULL
             , :RUECV007.CORIGE-REC
                   :WRK-B002-CORIGE-NULL
             , :RUECV007.CMODLD-RURAL
             , :RUECV007.VFINAN
             FROM  DB2PRD.CONTRATO_RURAL
             WHERE CCONTR              = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUFIB002'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2210'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'CONTRATO NAO ENCONTRADO'
                                       TO RUECWGEO-MENSAGEM
               MOVE 04                 TO RUECWGEO-COD-RETORNO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF WRK-B002-CFNALD-NULL       LESS ZEROS
              MOVE ZEROS   TO CFNALD-RURAL       OF RUECV007
           END-IF.

           IF WRK-B002-CATVDD-NULL       LESS ZEROS
              MOVE ZEROS   TO CATVDD-RURAL       OF RUECV007
           END-IF.

           IF WRK-B002-CORIGE-NULL       LESS ZEROS
              MOVE ZEROS   TO CORIGE-REC         OF RUECV007
           END-IF.

           IF WRK-B002-CPRODT-NULL       LESS ZEROS
              MOVE ZEROS   TO CPRODT-RURAL       OF RUECV007
           END-IF.

      *---------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2300-OBTER-SUBPROGRAMA          SECTION.
      *---------------------------------------------------------------*

           INITIALIZE RUECW824-REGISTRO.

           MOVE CORIGE-REC             OF  RUECV007
                                       TO  RUECW824-E-CORIGE-REC
           MOVE CFNALD-RURAL           OF  RUECV007
                                       TO  RUECW824-E-CFNALD-RURAL
           MOVE CPRODT-RURAL           OF  RUECV007
                                       TO  RUECW824-E-CPRODT-RURAL
           MOVE SPACES                 TO  RUECW824-E-RESERVA

           MOVE 'RUEC8824'             TO  WRK-MODULO.

           CALL  WRK-MODULO        USING   RUECW824-COPYBOOK
                                           ERRO-AREA
                                           SQLCA.

           IF  RUECW824-COD-RETORNO    NOT EQUAL ZEROS
               MOVE 04                 TO  RUECWGEO-COD-RETORNO
               MOVE 'SUBPROGRAMA NAO ENCONTRADO'
                                       TO RUECWGEO-MENSAGEM
               MOVE SPACES             TO RUECW824-S-SUB-PROG-RURAL
           END-IF.

      *---------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2400-ACESSAR-PARAMETRO          SECTION.
      *---------------------------------------------------------------*

           MOVE CATVDD-RURAL           OF RUECV007
                                       TO CATVDD-RURAL-INIC OF RURCB096
                                          CATVDD-RURAL-FNAL OF RURCB096.
           MOVE CFNALD-RURAL           OF RUECV007
                                       TO CFNALD-RURAL-INIC OF RURCB096
                                          CFNALD-RURAL-FNAL OF RURCB096.
           MOVE CPRODT-RURAL           OF RUECV007
                                       TO CPRODT-RURAL-INIC OF RURCB096
                                          CPRODT-RURAL-FNAL OF RURCB096.
           MOVE CORIGE-REC             OF RUECV007
                                       TO CORIGE-REC-INIC   OF RURCB096
                                          CORIGE-REC-FNAL   OF RURCB096.
           MOVE CMODLD-RURAL           OF RUECV007
                                       TO CMODLD-RURAL-INIC OF RURCB096
                                          CMODLD-RURAL-FNAL OF RURCB096.
           MOVE RUECW824-S-SUB-PROG-RURAL
                                       TO CSUB-PROG-INIC    OF RURCB096
                                          CSUB-PROG-FNAL    OF RURCB096.
           MOVE VFINAN                 OF RUECV007
                                       TO VMIN-FINAN-RURAL  OF RURCB096.

           EXEC SQL
             SELECT
               COUNT(*)
             INTO
               :WRK-COUNT
             FROM  DB2PRD.TPARM_CORND_GEOGR
             WHERE CATVDD_RURAL_INIC   <= :RURCB096.CATVDD-RURAL-INIC
             AND   CATVDD_RURAL_FNAL   >= :RURCB096.CATVDD-RURAL-FNAL
             AND   CFNALD_RURAL_INIC   <= :RURCB096.CFNALD-RURAL-INIC
             AND   CFNALD_RURAL_FNAL   >= :RURCB096.CFNALD-RURAL-FNAL
             AND   CORIGE_REC_INIC     <= :RURCB096.CORIGE-REC-INIC
             AND   CORIGE_REC_FNAL     >= :RURCB096.CORIGE-REC-FNAL
             AND   VMIN_FINAN_RURAL    <= :RURCB096.VMIN-FINAN-RURAL
             AND ((CPRODT_RURAL_INIC   <= :RURCB096.CPRODT-RURAL-INIC
             AND   CPRODT_RURAL_FNAL   >= :RURCB096.CPRODT-RURAL-FNAL )
             OR    CPRODT_RURAL_FNAL   IS NULL                        )
             AND ((CMODLD_RURAL_INIC   <= :RURCB096.CMODLD-RURAL-INIC
             AND   CMODLD_RURAL_FNAL   >= :RURCB096.CMODLD-RURAL-FNAL )
             OR    CMODLD_RURAL_FNAL   IS NULL                        )
             AND ((CSUB_PROG_INIC      <= :RURCB096.CSUB-PROG-INIC
             AND   CSUB_PROG_FNAL      >= :RURCB096.CSUB-PROG-FNAL    )
             OR    CSUB_PROG_FNAL      IS NULL                        )
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RURCB096'         TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2400'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2500-EXCLUIR-CORND-CEDULA       SECTION.
      *---------------------------------------------------------------*

           MOVE CJUNC-DEPDC OF RUECV007 TO CJUNC-DEPDC OF RUECB09M.
           MOVE CCDULA     OF RUECV007 TO  CCDULA      OF RUECB09M.

           PERFORM 2510-ABRIR-CSR01-RUECB09M.

           PERFORM 2520-LER-CSR01-RUECB09M.

           PERFORM UNTIL WRK-FIM-CSR01 EQUAL 'S'
               PERFORM 2520-LER-CSR01-RUECB09M
               PERFORM 2530-EXCLUIR-REG-RUECB09M
           END-PERFORM.

           PERFORM 2540-FECHAR-CSR01-RUECB09M.

      *---------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2510-ABRIR-CSR01-RUECB09M       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR01-RUECB09M
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09M'         TO ERR-DBD-TAB
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2510'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CSR01.

      *---------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2520-LER-CSR01-RUECB09M         SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-RUECB09M
             INTO
               :RUECB09M.CSEQ-MATR-RURAL
             , :RUECB09M.NSEQ-GLEBA-IMOV
             , :RUECB09M.NCORND-GEOGR-IMOV
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09M'         TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2520'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR01
           END-IF.

      *---------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2530-EXCLUIR-REG-RUECB09M       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             DELETE FROM DB2PRD.TCORND_GEOGR_CDULA
             WHERE  CURRENT OF CSR01-RUECB09M
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09M'         TO ERR-DBD-TAB
               MOVE 'DELETE'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2530'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2540-FECHAR-CSR01-RUECB09M      SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-RUECB09M
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09M'         TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2540'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2600-EXCLUIR-CORND-CONTR        SECTION.
      *---------------------------------------------------------------*

           MOVE CCONTR     OF RUECV007 TO CCONTR  OF RUECB09N.

           PERFORM 2610-ABRIR-CSR02-RUECB09N.

           PERFORM 2620-LER-CSR02-RUECB09N.

           PERFORM UNTIL WRK-FIM-CSR02 EQUAL 'S'
               PERFORM 2620-LER-CSR02-RUECB09N
               PERFORM 2630-EXCLUIR-REG-RUECB09M
           END-PERFORM.

           PERFORM 2640-FECHAR-CSR02-RUECB09N.

      *---------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2610-ABRIR-CSR02-RUECB09N       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR02-RUECB09N
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09N'         TO ERR-DBD-TAB
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2610'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CSR02.

      *---------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2620-LER-CSR02-RUECB09N         SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             FETCH CSR02-RUECB09N
             INTO
               :RUECB09M.CSEQ-MATR-RURAL
             , :RUECB09M.NSEQ-GLEBA-IMOV
             , :RUECB09M.NCORND-GEOGR-IMOV
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09N'         TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2620'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR02
           END-IF.

      *---------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2630-EXCLUIR-REG-RUECB09M       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             DELETE FROM DB2PRD.TCORND_GEOGR_CONTR
             WHERE  CURRENT OF CSR02-RUECB09N
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09N'         TO ERR-DBD-TAB
               MOVE 'DELETE'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2630'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2630-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2640-FECHAR-CSR02-RUECB09N      SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR02-RUECB09N
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 99                 TO RUECWGEO-COD-RETORNO
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09N'         TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '2640'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2640-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           MOVE  ERRO-AREA             TO  LNK-ERRO-AREA.
           MOVE  SQLCA(1:136)          TO  LNK-SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
