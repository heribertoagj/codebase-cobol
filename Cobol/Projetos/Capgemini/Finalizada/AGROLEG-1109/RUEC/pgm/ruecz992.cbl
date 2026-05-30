      *================================================================*
       IDENTIFICATION  DIVISION.
      *================================================================*
      *
       PROGRAM-ID.     RUECZ992.
       AUTHOR.         HEXA SOLUTION.
      *
      *----------------------------------------------------------------*
      * *** HEXAVISION - 01/2007 - CONVERSAO - EASYTRIEVE => COBOL *** *
      *       EASY  ->  RUEC0992   ******  COBOL  ->  RUECZ992         *
      *----------------------------------------------------------------*
      *
      *================================================================*
       ENVIRONMENT     DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION   SECTION.
      *----------------------------------------------------------------*
      *
      *    DECIMAL-POINT           IS          COMMA.
      *
      *----------------------------------------------------------------*
       INPUT-OUTPUT    SECTION.
      *----------------------------------------------------------------*
      *
       FILE-CONTROL.
      *
           SELECT      HISTDEVO    ASSIGN      TO          HISTDEVO
                       FILE STATUS IS          WRK-FS-HISTDEVO.
      *
           SELECT      RELATO      ASSIGN      TO          RELATO
                       FILE STATUS IS          WRK-FS-RELATO.
      *
      *================================================================*
       DATA            DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       FILE            SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    INPUT - ARQUIVO HISTDEVO   -   LRECL 1236
      *----------------------------------------------------------------*
      *
       FD  HISTDEVO    RECORDING   MODE        IS          F
                       BLOCK       CONTAINS    0           RECORDS
                       LABEL       RECORD      IS          STANDARD.
      *
       01  FD-REG-HISTDEVO             PIC  X(1236).
      *
      *----------------------------------------------------------------*
      *    OUTPUT - ARQUIVO RELATO   -   LRECL 133
      *----------------------------------------------------------------*
      *
       FD  RELATO      RECORDING   MODE        IS          F
                       BLOCK       CONTAINS    0           RECORDS
                       LABEL       RECORD      IS          STANDARD.
      *
       01  FD-REG-RELATO.
           03  FILLER                  PIC  X(001).
           03  FD-REG-RELATO-LIN       PIC  X(132).
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *
       77  FILLER                      PIC  X(030)         VALUE
           'FILE STATUS / FIM DE ARQUIVO'.
      *
       77  WRK-FS-HISTDEVO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.
       77  WRK-FIM-HISTDEVO            PIC  X(001)         VALUE 'N'.
      *
       77  FILLER                      PIC  X(030)         VALUE
           'VARIAVEIS AUXILIARES'.
      *
       77  WRK-ABEND                   PIC S9(004) COMP    VALUE +1234.
      *
       77  WRK-WACUM                   PIC  9(005)         VALUE 99.
       77  WRK-WPAG                    PIC  9(005)         VALUE ZEROS.
      *
       77  FILLER                      PIC  X(030)         VALUE
           'CONTADORES'.
       77  CT-REC-HISTDEVO             PIC S9(009) COMP-3  VALUE ZEROS.
      *
       01  FILLER                      PIC  X(030)         VALUE
           'VARIAVEIS AUXILIARES'.
      *
       01  WRK-SYSDATE-EDT.
           03  WRK-SYSDATE-EDT-DIA     PIC  Z9             VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-SYSDATE-EDT-MES     PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-SYSDATE-EDT-ANO     PIC  9(002)         VALUE ZEROS.
      *
       01  WRK-SYSDATE-LONG.
           03  FILLER                  PIC  9(002)         VALUE 20.
           03  WRK-SYSDATE             PIC  9(006)         VALUE ZEROS.
       01  FILLER                      REDEFINES   WRK-SYSDATE-LONG.
           03  WRK-SYSDATE-ANO         PIC  9(004).
           03  FILLER                  REDEFINES   WRK-SYSDATE-ANO.
             05  WRK-SYSDATE-ANO-SS    PIC  9(002).
             05  WRK-SYSDATE-ANO-AA    PIC  9(002).
           03  WRK-SYSDATE-MES         PIC  9(002).
      *
       01  WRK-SYSDATE-LONG-EDT.
           03  WRK-SYSDATE-LONG-EDT-DIA PIC 9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-SYSDATE-LONG-EDT-MES PIC 9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-SYSDATE-LONG-EDT-ANO PIC 9(004)         VALUE ZEROS.
      *
       01  FILLER                      PIC  X(030)         VALUE
           'RELATORIO'.
      *
       01  LC01-REG-RELATO.
           03  FILLER                  PIC  X(008)         VALUE
               'RUECZ992'.
           03  FILLER                  PIC  X(021)         VALUE SPACES.
           03  FILLER                  PIC  X(038)         VALUE
               'MIGRACAO TABELA DEVOLUCAO P/ HISTORICO'.
           03  FILLER                  PIC  X(012)         VALUE SPACES.
           03  LC01-RELATO-SYSDATE     PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(022)         VALUE SPACES.
           03  FILLER                  PIC  X(005)         VALUE
           03  LC01-RELATO-NUM-PAGE    PIC  9(005)         VALUE ZEROS.
      *
       01  LC02-REG-RELATO.
           03  FILLER                  PIC  X(028)         VALUE
               'JUNCAO    CEDULA  CONTRATO  '.
           03  FILLER                  PIC  X(041)         VALUE
               'DT.CEDULA  DT.DEVOL.           VL.FINANC.'.
      *
       01  LC03-REG-RELATO.
           03  FILLER                  PIC  X(029)         VALUE SPACES.
           03  FILLER                  PIC  X(019)         VALUE
               'MOTIVO DA DEVOLUCAO'.
      *
       01  LD01-REG-RELATO.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD01-JUNCAO             PIC  ZZZZZ          VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD01-CEDULA             PIC  ZZZZZZZZZ      VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD01-CCONTR             PIC  ZZZZZZZZZ      VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD01-DDEVLC             PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD01-VFINAN             PIC  Z,ZZZ,ZZZ,ZZZ,ZZZ.99 VALUE
               ZEROS.
      *
       01  LD02-REG-RELATO.
           03  FILLER                  PIC  X(029)         VALUE SPACES.
           03  LD02-RMOTVO             PIC  X(079)         VALUE SPACES.
      *
       01  FILLER                      PIC  X(030)         VALUE
           'LAYOUT DE ARQUIVOS'.
      *
       01  REG-HISTDEVO.
           03  CCONTR                  PIC  9(009) COMP-3.
           03  CSEQ-HIST-DEVLC         PIC  9(003) COMP-3.
           03  DDEVLC                  PIC  X(010).
           03  VFINAN                  PIC S9(013)V9(02) COMP-3.
           03  FILLER                  PIC  X(001).
           03  CFUNC-BDSCO             PIC  9(009) COMP-3.
           03  RMOTVO-LEN              PIC  9(004) COMP.
             05  RMOTVO                PIC  X(079)
                                       OCCURS  15  TIMES.
           03  CEDULA                  PIC  9(009) COMP-3.
           03  JUNCAO                  PIC  9(005) COMP-3.
           03  DCEDULA                 PIC  X(010).
      *
      *================================================================*
       PROCEDURE       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       0000-PRINCIPAL  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM     1000-INICIO.
      *
           PERFORM     2000-PROCESSA-HISTDEVO
             UNTIL     WRK-FIM-HISTDEVO EQUAL 'S'.
      *
      *
           STOP RUN.
      *
       0000-EXIT.      EXIT.
      *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       1000-INICIO     SECTION.
      *----------------------------------------------------------------*
      *
           ACCEPT      WRK-SYSDATE FROM        DATE.
           MOVE        WRK-SYSDATE-DIA TO      WRK-SYSDATE-EDT-MES.
           MOVE        WRK-SYSDATE-MES TO      WRK-SYSDATE-EDT-DIA.
           MOVE        WRK-SYSDATE-ANO-AA TO   WRK-SYSDATE-EDT-ANO.
           MOVE        WRK-SYSDATE-DIA TO      WRK-SYSDATE-LONG-EDT-DIA.
           MOVE        WRK-SYSDATE-MES TO      WRK-SYSDATE-LONG-EDT-MES.
           MOVE        WRK-SYSDATE-ANO TO      WRK-SYSDATE-LONG-EDT-ANO.
           MOVE        WRK-SYSDATE-EDT TO      LC01-RELATO-SYSDATE.
      *
           OPEN        INPUT       HISTDEVO.
           IF          WRK-FS-HISTDEVO NOT EQUAL '00'
               DISPLAY 'PROGRAMA RUEC0992 - ERRO OPEN '
               DISPLAY 'ARQUIVO  HISTDEVO - FS - ' WRK-FS-HISTDEVO
               PERFORM 9900-FINALIZAR-ERRO
           END-IF.
      *
           OPEN        OUTPUT      RELATO.
      *
           IF          WRK-FS-RELATO NOT EQUAL '00'
               DISPLAY 'PROGRAMA RUEC0992 - ERRO OPEN '
               DISPLAY 'ARQUIVO  RELATO - FS - ' WRK-FS-RELATO
               PERFORM 9900-FINALIZAR-ERRO
           END-IF.
      *
           PERFORM     1100-LER-HISTDEVO.
      *
       1000-EXIT.      EXIT.
      *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *
           READ        HISTDEVO    INTO        REG-HISTDEVO.
      *
           IF          WRK-FS-HISTDEVO EQUAL   '10'
               MOVE    'S'         TO          WRK-FIM-HISTDEVO
           ELSE
           IF          WRK-FS-HISTDEVO NOT EQUAL '00'
               DISPLAY 'PROGRAMA RUEC0992 - ERRO READ '
               DISPLAY 'ARQUIVO  HISTDEVO - FS - ' WRK-FS-HISTDEVO
               PERFORM 9900-FINALIZAR-ERRO
           ELSE
               ADD     1           TO          CT-REC-HISTDEVO
           END-IF
           END-IF.
      *
       1100-EXIT.      EXIT.
      *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *
           IF          WRK-WACUM   GREATER     70
               ADD     1           TO          WRK-WPAG
               PERFORM 7010-CABECALHO-RELATO
               MOVE    4           TO          WRK-WACUM
           END-IF.
      *
           ADD         17          TO          WRK-WACUM.
      *
           PERFORM     7000-PRINT-RELATO.
      *
           PERFORM     1100-LER-HISTDEVO.
      *
       2000-EXIT.      EXIT.
      *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       7000-PRINT-RELATO           SECTION.
      *----------------------------------------------------------------*
           MOVE        JUNCAO      TO          LD01-JUNCAO.
           MOVE        CEDULA      TO          LD01-CEDULA.
           MOVE        CCONTR      TO          LD01-CCONTR.
           MOVE        DCEDULA     TO          LD01-DCEDULA.
           MOVE        DDEVLC      TO          LD01-DDEVLC.
           MOVE        VFINAN      TO          LD01-VFINAN.
      *
           MOVE        LD01-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(01)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(02)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(04)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(05)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(06)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(07)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(08)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(09)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(10)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(11)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
           MOVE        RMOTVO(12)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(13)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(14)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        RMOTVO(15)  TO          LD02-RMOTVO.
           MOVE        LD02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        SPACES      TO          FD-REG-RELATO-LIN.
           PERFORM     7095-FS-RELATO.
      *
       7000-EXIT.      EXIT.
      *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       7010-CABECALHO-RELATO       SECTION.
      *----------------------------------------------------------------*
      *
           MOVE        SPACES      TO          FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     PAGE.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        WRK-WPAG    TO          LC01-RELATO-NUM-PAGE.
           MOVE        LC01-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        SPACES      TO          FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
      *
           MOVE        LC02-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        LC03-REG-RELATO TO      FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
           MOVE        SPACES      TO          FD-REG-RELATO-LIN.
           WRITE       FD-REG-RELATO AFTER     1.
           PERFORM     7095-FS-RELATO.
      *
       7010-EXIT.      EXIT.
      *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       7095-FS-RELATO  SECTION.
      *----------------------------------------------------------------*
      *
               DISPLAY 'PROGRAMA RUEC0992 - ERRO READ '
               DISPLAY 'ARQUIVO  RELATO - FS - ' WRK-FS-RELATO
               PERFORM 9900-FINALIZAR-ERRO
           END-IF.
      *
       7095-EXIT.      EXIT.
      *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       9000-FINALIZAR  SECTION.
      *----------------------------------------------------------------*
      *
           CLOSE       HISTDEVO.
      *
           IF          WRK-FS-HISTDEVO NOT EQUAL '00'
               DISPLAY 'PROGRAMA RUEC0992 - ERRO CLOSE '
               DISPLAY 'ARQUIVO  HISTDEVO - FS - ' WRK-FS-HISTDEVO
               PERFORM 9900-FINALIZAR-ERRO
           END-IF.
      *
      *
           IF          WRK-FS-RELATO NOT EQUAL '00'
               DISPLAY 'PROGRAMA RUEC0992 - ERRO CLOSE '
               DISPLAY 'ARQUIVO  RELATO - FS - ' WRK-FS-RELATO
               PERFORM 9900-FINALIZAR-ERRO
           END-IF.
      *
       9000-EXIT.      EXIT.
      *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       9900-FINALIZAR-ERRO         SECTION.
      *----------------------------------------------------------------*
      *
           MOVE        '12'        TO          RETURN-CODE.
      *
           CALL        'ILBOABN0'  USING       WRK-ABEND.
      *
       9900-EXIT.      EXIT.
      *
