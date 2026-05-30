      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID.    GFCT3022.
      *****************************************************************
      *****************************************************************
      *                                                               *
      *PROGRAMA    : GFCT3022.                                        *
      *PROGRAMADOR : PAGNOCCA - SONDA PROCWORK    - DDS - GP 50.      *
      *ANALISTA    : PAGNOCCA - SONDA PROCWORK    - DDS - GP 50.      *
      *DATA        : 03/10/2011.                                      *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      *OBJETIVO    : BALANCE LINE ENTRE ARQUIVO DE LOAD DA TABELA     *
      *--------      GFCTB092 (ATUALIZADO) E O ARQUIVO DE HPU DA      *
      *              TABELA GFCTB0N2, GERANDO ARQUIVO QUE FARA LOAD   *
      *              NA TABELA GFCTB0N2 (ATUALIZADO).                 *
      *                                                               *
      *ENTRADAS    : ->  LOADB092 - CONVENCIONAL - LRECL=187.         *
      *--------          BAIXB0N2 -      "       - LRECL=115.         *
      *                                                               *
      *SAIDA       : ->  LOADB0N2 -      "       - LRECL=115.         *
      *-----                                                          *
      *                                                               *
      *****************************************************************

      **********************
       ENVIRONMENT DIVISION.
      **********************

       CONFIGURATION SECTION.
       SPECIAL-NAMES.

           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  LOADB092  ASSIGN TO UT-S-LOADB092
                       FILE  STATUS IS WRK-FS-LOADB092.

           SELECT  BAIXB0N2  ASSIGN TO UT-S-BAIXB0N2
                       FILE  STATUS IS WRK-FS-BAIXB0N2.

           SELECT  LOADB0N2  ASSIGN TO UT-S-LOADB0N2
                       FILE  STATUS IS WRK-FS-LOADB0N2.

      ***************
       DATA DIVISION.
      ***************

       FILE SECTION.

      *---------------------------------------------------------------*
      *ARQUIVO LOADB092 - ARQUIVO FIXO - INPUT - LRECL=187            *
      *CARGA ATUALIZADA NA TABELA GFCTB0N2                            *
      *---------------------------------------------------------------*

       FD  LOADB092 RECORDING F LABEL RECORD STANDARD BLOCK 0.

       01  FD-LOADB092                   PIC X(187).

      *---------------------------------------------------------------*
      *ARQUIVO BAIXB0N2 - ARQUIVO FIXO - INPUT - LRECL=115            *
      *HPU DA TABELA GFCTB0N2                                         *
      *---------------------------------------------------------------*

       FD  BAIXB0N2 RECORDING F LABEL RECORD STANDARD BLOCK 0.

       01  FD-BAIXB0N2                   PIC X(115).

      *---------------------------------------------------------------*
      *ARQUIVO LOADB0N2 - ARQUIVO FIXO - OUTPUT                       *
      *LOAD NA TABELA GFCTB0N2  - LRECL=187                           *
      *---------------------------------------------------------------*

       FD  LOADB0N2 RECORDING F LABEL RECORD STANDARD BLOCK 0.

       01  FD-LOADB0N2                   PIC X(115).

      *-----------------------*
       WORKING-STORAGE SECTION.
      *-----------------------*

      *---------------------------------------------------------------*
      *AREAS PARA CANCELAMENTO DE PROGRAMA                            *
      *---------------------------------------------------------------*

       77  WRK-ABEND                     PIC S9(04) COMP-3 VALUE +1111.

      *---------------------------------------------------------------*
      *AREAS DE TRABALHO                                              *
      *---------------------------------------------------------------*

       77  ACU-LIDOS-LOADB092            PIC 9(10)  COMP-3 VALUE ZEROS.
       77  ACU-LIDOS-BAIXB0N2            PIC 9(10)  COMP-3 VALUE ZEROS.
       77  ACU-GRAV-LOADB0N2             PIC 9(10)  COMP-3 VALUE ZEROS.
       77  ACU-DESP-BAIXB0N2             PIC 9(10)  COMP-3 VALUE ZEROS.
       77  ACU-DESP-LOADB092             PIC 9(10)  COMP-3 VALUE ZEROS.

       77  WRK-EDIT-LIDOS-LOADB092       PIC Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-EDIT-LIDOS-BAIXB0N2       PIC Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-EDIT-GRAV-LOADB0N2        PIC Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-EDIT-DESP-BAIXB0N2        PIC Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-EDIT-DESP-LOADB092        PIC Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

      *---------------------------------------------------------------*
      *FILE STATUS                                                    *
      *---------------------------------------------------------------*

       01  FILLER.
           03  WRK-FS-LOADB092           PIC X(02)         VALUE SPACES.
           03  WRK-FS-BAIXB0N2           PIC X(02)         VALUE SPACES.
           03  WRK-FS-LOADB0N2           PIC X(02)         VALUE SPACES.

      *---------------------------------------------------------------*
      *CHAVE DOS ARQUIVOS                                             *
      *---------------------------------------------------------------*

           03  WRK-CHV-LOADB092.
               05  WRK-CHV-B092-CROTNA   PIC X(04)         VALUE SPACES.
               05  WRK-CHV-B092-DENVIO.
                   10 WRK-CHV-B092-DENVIO-A PIC 9(04)    VALUE 0.
                   10 WRK-CHV-B092-DENVIO-M PIC 9(02)    VALUE 0.
                   10 WRK-CHV-B092-DENVIO-D PIC 9(02)    VALUE 0.
               05  WRK-CHV-B092-CNRO-ARQ PIC S9(02) COMP-3 VALUE ZEROS.
               05  WRK-CHV-B092-CSEQ     PIC S9(11) COMP-3 VALUE ZEROS.

           03  WRK-CHV-BAIXB0N2.
               05  WRK-CHV-B0N2-CROTNA   PIC X(04)         VALUE SPACES.
               05  WRK-CHV-B0N2-DENVIO.
                   10 WRK-CHV-BON2-DENVIO-A PIC 9(04)    VALUE 0.
                   10 WRK-CHV-BON2-DENVIO-M PIC 9(02)    VALUE 0.
                   10 WRK-CHV-BON2-DENVIO-D PIC 9(02)    VALUE 0.
               05  WRK-CHV-B0N2-CNRO-ARQ PIC S9(02) COMP-3 VALUE ZEROS.
               05  WRK-CHV-B0N2-CSEQ     PIC S9(11) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
      *LAYOUT DOS ARQUIVOS                                            *
      *---------------------------------------------------------------*

           03  WRK-REG-LOADB092.
               05  WRK-B092-CROTNA       PIC X(04)         VALUE SPACES.
               05  WRK-B092-DENVIO.
                   10 WRK-B092-DENVIO-D  PIC 9(02)         VALUE ZEROS.
                   10 FILLER             PIC X(01)         VALUE SPACE.
                   10 WRK-B092-DENVIO-M  PIC 9(02)         VALUE ZEROS.
                   10 FILLER             PIC X(01)         VALUE SPACE.
                   10 WRK-B092-DENVIO-A  PIC 9(04)         VALUE ZEROS.
               05  WRK-B092-CNRO-ARQ     PIC S9(02) COMP-3 VALUE ZEROS.
               05  WRK-B092-CSEQ         PIC S9(11) COMP-3 VALUE ZEROS.
               05  FILLER                PIC X(165)        VALUE SPACES.

           03  WRK-REG-N2.
               05  FILLER                PIC X(93).
               05  WRK-REG-BAIXB0N2.
                10  WRK-B0N2-CROTNA       PIC X(04)        VALUE SPACES.
                10  WRK-B0N2-DENVIO.
                    15 WRK-B0N2-DENVIO-D  PIC 9(02)        VALUE ZEROS.
                    15 FILLER             PIC X(01)        VALUE SPACE.
                    15 WRK-B0N2-DENVIO-M  PIC 9(02)        VALUE ZEROS.
                    15 FILLER             PIC X(01)        VALUE SPACE.
                    15 WRK-B0N2-DENVIO-A  PIC 9(04)        VALUE ZEROS.
                10  WRK-B0N2-CNRO-ARQ    PIC S9(02) COMP-3 VALUE ZEROS.
                10  WRK-B0N2-CSEQ        PIC S9(11) COMP-3 VALUE ZEROS.

      ********************
       PROCEDURE DIVISION.
      ********************

      *---------------------------------------------------------------*
      *PROCEDIMENTOS INICIAIS                                         *
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0010-INICIO SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT  LOADB092
                       BAIXB0N2
               OUTPUT  LOADB0N2.

           IF    WRK-FS-LOADB092  NOT EQUAL '00'
                 DISPLAY '************** GFCT3022 *************'
                 DISPLAY '*                                   *'
                 DISPLAY '*     ERRO NA ABERTURA DO ARQUIVO   *'
                 DISPLAY '*              LOADB092             *'
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-LOADB092
                                                    '         *'
                 DISPLAY '*                                   *'
                 DISPLAY '************** GFCT3022 *************'

                 CALL    'ILBOABN0'  USING WRK-ABEND
           END-IF.

           IF    WRK-FS-BAIXB0N2  NOT EQUAL '00'
                 DISPLAY '************** GFCT3022 *************'
                 DISPLAY '*                                   *'
                 DISPLAY '*     ERRO NA ABERTURA DO ARQUIVO   *'
                 DISPLAY '*              BAIXB0N2             *'
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-BAIXB0N2
                                                    '         *'
                 DISPLAY '*                                   *'
                 DISPLAY '************** GFCT3022 *************'

                 CALL    'ILBOABN0'  USING WRK-ABEND
           END-IF.

           IF    WRK-FS-LOADB0N2  NOT EQUAL '00'
                 DISPLAY '************** GFCT3022 *************'
                 DISPLAY '*                                   *'
                 DISPLAY '*     ERRO NA ABERTURA DO ARQUIVO   *'
                 DISPLAY '*              LOADB0N2             *'
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-LOADB0N2
                                                    '         *'
                 DISPLAY '*                                   *'
                 DISPLAY '************** GFCT3022 *************'

                 CALL    'ILBOABN0'  USING WRK-ABEND
           END-IF.

           PERFORM  0100-LER-LOADB092.

           IF    WRK-FS-LOADB092  EQUAL  '10'
                 DISPLAY '************** GFCT3022 *************'
                 DISPLAY '*                                   *'
                 DISPLAY '*        ARQUIVO LOADB092 VAZIO     *'
                 DISPLAY '*        ----------------------     *'
                 DISPLAY '*                                   *'
                 DISPLAY '*          PROGRAMA ENCERRADO       *'
                 DISPLAY '*                                   *'
                 DISPLAY '************** GFCT3022 *************'
                 PERFORM  9999-FINALIZA
           END-IF.

           PERFORM  0200-LER-BAIXB0N2.

           IF    WRK-FS-BAIXB0N2  EQUAL  '10'
                 DISPLAY '************** GFCT3022 *************'
                 DISPLAY '*                                   *'
                 DISPLAY '*       ARQUIVO BAIXB0N2 VAZIO      *'
                 DISPLAY '*       ----------------------      *'
                 DISPLAY '*                                   *'
                 DISPLAY '*        PROGRAMA ENCERRADO         *'
                 DISPLAY '*                                   *'
                 DISPLAY '************** GFCT3022 *************'
                 PERFORM  9999-FINALIZA
           END-IF.

           PERFORM  1000-BALANCE-LINE
                    UNTIL  WRK-FS-BAIXB0N2  EQUAL '10'.

           MOVE  ACU-LIDOS-LOADB092  TO  WRK-EDIT-LIDOS-LOADB092.
           MOVE  ACU-LIDOS-BAIXB0N2  TO  WRK-EDIT-LIDOS-BAIXB0N2.
           MOVE  ACU-GRAV-LOADB0N2   TO  WRK-EDIT-GRAV-LOADB0N2.
           MOVE  ACU-DESP-BAIXB0N2   TO  WRK-EDIT-DESP-BAIXB0N2.
           MOVE  ACU-DESP-LOADB092   TO  WRK-EDIT-DESP-LOADB092.

           DISPLAY '*************** GFCT3022 ****************'
           DISPLAY '*                                       *'
           DISPLAY '*          TOTAIS DE CONTROLE           *'
           DISPLAY '*          ------------------           *'
           DISPLAY '*                                       *'
           DISPLAY '* LIDOS     (LOADB092): ' WRK-EDIT-LIDOS-LOADB092
                                                        '   *'
           DISPLAY '* LIDOS     (BAIXB0N2): ' WRK-EDIT-LIDOS-BAIXB0N2
                                                        '   *'
           DISPLAY '* EXCLUIDOS (BAIXB0N2): ' WRK-EDIT-DESP-BAIXB0N2
                                                        '   *'
           DISPLAY '* GRAVADOS  (LOADB0N2): ' WRK-EDIT-GRAV-LOADB0N2
                                                        '   *'
           DISPLAY '*                                       *'
           DISPLAY '*************** GFCT3022 ****************'

           PERFORM 9999-FINALIZA.

      *---------------------------------------------------------------*
       0010-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *LEITURA ARQUIVO BAIXADO VIA HPU - TABELA GFCTB092 APOS LOAD    *
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0100-LER-LOADB092 SECTION.
      *---------------------------------------------------------------*

           READ  LOADB092  INTO  WRK-REG-LOADB092.

           IF  WRK-FS-LOADB092          EQUAL  '10'
               MOVE  HIGH-VALUES          TO   WRK-CHV-LOADB092
               GO                         TO   0100-99-FIM
           END-IF.

           IF  WRK-FS-LOADB092          EQUAL  '00'
               MOVE  WRK-B092-CROTNA      TO   WRK-CHV-B092-CROTNA
               MOVE  WRK-B092-DENVIO(7:4) TO   WRK-CHV-B092-DENVIO(1:4)
               MOVE  WRK-B092-DENVIO(4:2) TO   WRK-CHV-B092-DENVIO(5:2)
               MOVE  WRK-B092-DENVIO(1:2) TO   WRK-CHV-B092-DENVIO(7:2)
               MOVE  WRK-B092-CNRO-ARQ    TO   WRK-CHV-B092-CNRO-ARQ
               MOVE  WRK-B092-CSEQ        TO   WRK-CHV-B092-CSEQ
               ADD   1                    TO   ACU-LIDOS-LOADB092
               GO                         TO   0100-99-FIM
           END-IF.

           DISPLAY  '************** GFCT3022 *************'.
           DISPLAY  '*                                   *'.
           DISPLAY  '*     ERRO NA LEITURA DO ARQUIVO    *'.
           DISPLAY  '*              LOADB092             *'.
           DISPLAY  '*         FILE STATUS =  ' WRK-FS-LOADB092
                                               '         *'.
           DISPLAY  '*                                   *'.
           DISPLAY  '************** GFCT3022 *************'.

           CALL     'ILBOABN0'  USING WRK-ABEND.

      *---------------------------------------------------------------*
       0100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *LEITURA ARQUIVO BAIXADO VIA HPU - GFCTB0N2 ANTES DO LOAD       *
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0200-LER-BAIXB0N2 SECTION.
      *---------------------------------------------------------------*

           READ  BAIXB0N2  INTO  WRK-REG-N2.

           IF  WRK-FS-BAIXB0N2          EQUAL  '10'
               GO                         TO   0200-99-FIM
           END-IF.

           IF  WRK-FS-BAIXB0N2          EQUAL  '00'
               MOVE  WRK-B0N2-CROTNA      TO   WRK-CHV-B0N2-CROTNA
               MOVE  WRK-B0N2-DENVIO(7:4) TO   WRK-CHV-B0N2-DENVIO(1:4)
               MOVE  WRK-B0N2-DENVIO(4:2) TO   WRK-CHV-B0N2-DENVIO(5:2)
               MOVE  WRK-B0N2-DENVIO(1:2) TO   WRK-CHV-B0N2-DENVIO(7:2)
               MOVE  WRK-B0N2-CNRO-ARQ    TO   WRK-CHV-B0N2-CNRO-ARQ
               MOVE  WRK-B0N2-CSEQ        TO   WRK-CHV-B0N2-CSEQ
               ADD   1                    TO   ACU-LIDOS-BAIXB0N2
               GO                         TO   0200-99-FIM
           END-IF.

           DISPLAY  '************** GFCT3022 *************'.
           DISPLAY  '*                                   *'.
           DISPLAY  '*     ERRO NA LEITURA DO ARQUIVO    *'.
           DISPLAY  '*              BAIXB0N2             *'.
           DISPLAY  '*         FILE STATUS =  ' WRK-FS-BAIXB0N2
                                               '         *'.
           DISPLAY  '*                                   *'.
           DISPLAY  '************** GFCT3022 *************'.

           CALL     'ILBOABN0'  USING WRK-ABEND.

      *---------------------------------------------------------------*
       0200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *COMPARA ARQUIVOS - GRAVA SOMENTE CHAVES IGUAIS                 *
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-BALANCE-LINE SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CHV-LOADB092  EQUAL  WRK-CHV-BAIXB0N2
               PERFORM  1100-GRAVA-LOADB0N2
               PERFORM  0200-LER-BAIXB0N2
               GO       TO  1000-99-FIM
           END-IF.

           IF  WRK-CHV-LOADB092  LESS   WRK-CHV-BAIXB0N2
               PERFORM  0100-LER-LOADB092
               GO       TO  1000-99-FIM
           END-IF.

           ADD  1       TO  ACU-DESP-BAIXB0N2.

           PERFORM  0200-LER-BAIXB0N2.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *GRAVACAO DO ARQUIVO LOADB092 ATUALIZADO PARA LOAD NA GFCTB0N2  *
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1100-GRAVA-LOADB0N2 SECTION.
      *---------------------------------------------------------------*

           WRITE  FD-LOADB0N2   FROM  WRK-REG-N2.

           ADD    1              TO   ACU-GRAV-LOADB0N2.

           IF   WRK-FS-LOADB0N2  NOT EQUAL  '00'
                DISPLAY  '************** GFCT3022 *************'
                DISPLAY  '*                                   *'
                DISPLAY  '*     ERRO NA GRAVACAO DO ARQUIVO   *'
                DISPLAY  '*              LOADB0N2             *'
                DISPLAY  '*         FILE STATUS =  ' WRK-FS-LOADB0N2
                                                    '         *'
                DISPLAY  '*                                   *'
                DISPLAY  '************** GFCT3022 *************'

                CALL     'ILBOABN0'  USING  WRK-ABEND
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *FECHAMENTO DOS ARQUIVOS                                        *
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-FINALIZA SECTION.
      *---------------------------------------------------------------*

           CLOSE   LOADB092
                   BAIXB0N2
                   LOADB0N2.

           IF    WRK-FS-LOADB092  NOT EQUAL '00'
                 DISPLAY '************** GFCT3022 *************'
                 DISPLAY '*                                   *'
                 DISPLAY '*   ERRO NO FECHAMENTO DO ARQUIVO   *'
                 DISPLAY '*              LOADB0N2             *'
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-LOADB0N2
                                                    '         *'
                 DISPLAY '*                                   *'
                 DISPLAY '************** GFCT3022 *************'

                 CALL    'ILBOABN0'  USING WRK-ABEND
           END-IF.

           IF    WRK-FS-BAIXB0N2  NOT EQUAL '00'
                 DISPLAY '************** GFCT3022 *************'
                 DISPLAY '*                                   *'
                 DISPLAY '*   ERRO NO FECHAMENTO DO ARQUIVO   *'
                 DISPLAY '*              BAIXB0N2             *'
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-BAIXB0N2
                                                    '         *'
                 DISPLAY '*                                   *'
                 DISPLAY '************** GFCT3022 *************'

                 CALL    'ILBOABN0'  USING WRK-ABEND
           END-IF.

           IF    WRK-FS-LOADB0N2  NOT EQUAL '00'
                 DISPLAY '************** GFCT3022 *************'
                 DISPLAY '*                                   *'
                 DISPLAY '*   ERRO NO FECHAMENTO DO ARQUIVO   *'
                 DISPLAY '*              LOADB0N2             *'
                 DISPLAY '*         FILE STATUS =  ' WRK-FS-LOADB0N2
                                                    '         *'
                 DISPLAY '*                                   *'
                 DISPLAY '************** GFCT3022 *************'

                 CALL    'ILBOABN0'  USING WRK-ABEND
           END-IF.

           STOP    RUN.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
