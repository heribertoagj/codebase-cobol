      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID.    BVVE0551.
       AUTHOR.        FRANCISMARA.

      *================================================================*
      *                   B S I  T E C N O L O G I A                   *
      *----------------------------------------------------------------*
      *                                                                *
      *      PROGRAMADOR  : FRANCISMARA         - BSI                  *
      *      ANALISTA     : FRANCISMARA         - BSI                  *
      *      DATA         : ABRIL/2017                                 *
      *                                                                *
      *      OBJETIVO     :                                            *
      *        CALCULA OS VALORES QUE DEVEM SER ATUALZIADOS NAS AGENDAS*
      *        PARA CLIENTES COM SALDO NEGATIVO - TRAG                 *
      *                                                                *
      *      ARQUIVOS                                                  *
      *                                                                *
      *      INPUT :                                                   *
      *                                                                *
      *      AGENDOLD            - AGENDA DAS CONTAS ANTIGAS COM TRAG  *
      *      AGENDNEW            - AGENDA DAS CONTAS NOVAS COM TRAG    *
      *                                                                *
      *      OUTPUT :                                                  *
      *                                                                *
      *      AGENDATU            - AGENDA DE CLIENTES ATUALIZADA       *
      *      RELATUAL            - LISTA MANUTENCOES                   *
      *                                                                *
      *================================================================*

      ******************************************************************
       ENVIRONMENT                     DIVISION.
      ******************************************************************

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.

           SELECT  AGENDOLD   ASSIGN  TO  UT-S-AGENDOLD
                                  FILE STATUS IS WRK-FS-AGENDOLD.

           SELECT  AGENDNEW   ASSIGN  TO  UT-S-AGENDNEW
                                  FILE STATUS IS WRK-FS-AGENDNEW.

           SELECT  AGENDATU   ASSIGN  TO  UT-S-AGENDATU
                                  FILE STATUS IS WRK-FS-AGENDATU.

           SELECT  RELATUAL   ASSIGN  TO  UT-S-RELATUAL
                                  FILE STATUS IS WRK-FS-RELATUAL.

      ******************************************************************
       DATA                            DIVISION.
      ******************************************************************

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ARQUIVO DE CLIENTES CONTA ORIGEM        - ENTRADA LRECL = 045  *
      *----------------------------------------------------------------*

       FD  AGENDOLD
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01  FD-REG-AGENDOLD             PIC X(045).

      *----------------------------------------------------------------*
      * BARQUIVO DE CLIENTES CONTA DESTINO      - ENTRADA LRECL = 038  *
      *----------------------------------------------------------------*

       FD  AGENDNEW
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01  FD-REG-AGENDNEW             PIC X(038).

      *----------------------------------------------------------------*
      * AGENDA CLIENTES NEGATIVOS               - SAIDA   LRECL = 028  *
      *----------------------------------------------------------------*

       FD  AGENDATU
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01  FD-REG-AGENDATU             PIC X(028).

      *----------------------------------------------------------------*
      * LISTA CLIENTES COM TRAG EM CONTA        - SAIDA   LRECL = 133  *
      *----------------------------------------------------------------*

       FD  RELATUAL
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01  FD-REG-RELATUAL             PIC X(133).

      ******************************************************************
       WORKING-STORAGE                 SECTION.
      ******************************************************************

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** INICIO DA WORKING-STORAGE ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS PARA TESTE DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-AGENDOLD             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-AGENDNEW             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-AGENDATU             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-RELATUAL             PIC  X(02)          VALUE SPACES.

       77  WRK-BATCH                   PIC  X(08)          VALUE
           'BATCH'.
       77  WRK-FECHAMENTO              PIC  X(13)          VALUE
           'NO FECHAMENTO'.
       77  WRK-ABERTURA                PIC  X(13)          VALUE
           'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13)          VALUE
           'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13)          VALUE
           'NA GRAVACAO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE MENSAGENS DE ERRO - FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(08)          VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(13)          VALUE SPACES.
           03  FILLER                  PIC  X(12)          VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(08)          VALUE SPACES.
           03  FILLER                  PIC  X(18)          VALUE
               ' - FILE STATUS =  '.
           03  WRK-FILE-STATUS         PIC  X(02)          VALUE SPACES.
           03  FILLER                  PIC  X(03)          VALUE ' **'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA MENSAGEM DE ARQUIVO VAZIO ***'.
      *----------------------------------------------------------------*

       01  WRK-ARQ-VAZIO               PIC  X(08)          VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA TRATAMENTO DE ERRO ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** LAYOUTS AGENDNEW ***'.
      *----------------------------------------------------------------*

       01  WRK-AGENDNEW.
           03 WRK-NEW-CJUNC-DEPDC        PIC S9(5)  COMP-3 VALUE ZEROS.
           03 WRK-NEW-CCTA-CORR          PIC S9(7)  COMP-3 VALUE ZEROS.
           03 WRK-NEW-CIDTFD-PRODT-CATAO PIC S9(2)  COMP-3 VALUE ZEROS.
           03 WRK-NEW-CIDTFD-INDCD       PIC S9(5)  COMP-3 VALUE ZEROS.
           03 WRK-NEW-DMOVTO             PIC X(10)         VALUE SPACES.
           03 WRK-NEW-VMOVTO-CREDT-CATAO PIC S9(13)V9(2) COMP-3 VALUE 0.
           03 WRK-NEW-VMOVTO-DEB-CATAO   PIC S9(13)V9(2) COMP-3 VALUE 0.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** LAYOUTS AGENDOLD ***'.
      *----------------------------------------------------------------*

       01  WRK-AGENDOLD.
          03 WRK-OLD-CJUNC-DEPDC        PIC S9(5)  COMP-3 VALUE ZEROS.
          03 WRK-OLD-CCTA-CORR          PIC S9(7)  COMP-3 VALUE ZEROS.
          03 WRK-OLD-CIDTFD-PRODT-CATAO PIC S9(2)  COMP-3 VALUE ZEROS.
          03 WRK-OLD-CIDTFD-INDCD       PIC S9(5)  COMP-3 VALUE ZEROS.
          03 WRK-OLD-DMOVTO             PIC X(10)         VALUE SPACES.
          03 WRK-OLD-VMOVTO-CREDT-CATAO PIC S9(13)V9(2) COMP-3 VALUE 0.
          03 WRK-OLD-VMOVTO-DEB-CATAO   PIC S9(13)V9(2) COMP-3 VALUE 0.
          03 WRK-OLD-AGENCNEW           PIC  9(05)  COMP-3  VALUE ZEROS.
          03 WRK-OLD-CONTANEW           PIC  9(07)  COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** LAYOUTS AGENDATU ***'.
      *----------------------------------------------------------------*

       01  WRK-AGENDATU.
          03 WRK-ATU-CJUNC-DEPDC        PIC S9(5)  COMP-3 VALUE ZEROS.
          03 WRK-ATU-CCTA-CORR          PIC S9(7)  COMP-3 VALUE ZEROS.
          03 WRK-ATU-CIDTFD-PRODT-CATAO PIC S9(2)  COMP-3 VALUE ZEROS.
          03 WRK-ATU-DMOVTO             PIC X(10)         VALUE SPACES.
          03 WRK-ATU-VMOVTO-DEB-CATAO   PIC S9(13)V9(2) COMP-3 VALUE 0.
          03 WRK-ATU-MANUTENCAO         PIC  X(01)        VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS-AGENDOLD          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-LIDOS-AGENDNEW          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-GRAVA-AGENDATU          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-GRAVA-RELATUAL          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-PAGINAS                 PIC  9(05)  COMP-3  VALUE ZEROS.
       01  ACU-LINHAS                  PIC  9(02)  COMP-3  VALUE 90.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-MASC                    PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

       01 WRK-HEXA.
          03 WRK-SINALIZADO          PIC +9(7)          VALUE ZEROS.
          03 FILLER REDEFINES WRK-SINALIZADO.
             05  FILLER              PIC X(03).
             05  WRK-9-5             PIC 9(05).
          03 FILLER REDEFINES WRK-SINALIZADO.
             05  FILLER              PIC X(01).
             05  WRK-9-7             PIC 9(07).
          03 FILLER REDEFINES WRK-SINALIZADO.
             05  FILLER              PIC X(06).
             05  WRK-9-2             PIC 9(02).
          03 WRK-13-SINALIZADO       PIC +9(13)V99      VALUE ZEROS.
          03 FILLER REDEFINES WRK-13-SINALIZADO.
             05  FILLER              PIC X(05).
             05  WRK-9-9V99          PIC 9(09)V99.
          03 FILLER REDEFINES WRK-13-SINALIZADO.
             05  FILLER              PIC X(11).
             05  WRK-9-3V99          PIC 9(03)V99.

       01  WRK-DATA-INV.
           05  WRK-ANO                 PIC  9(04)          VALUE ZEROS.
           05  WRK-MES                 PIC  9(02)          VALUE ZEROS.
           05  WRK-DIA                 PIC  9(02)          VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES
           WRK-DATA-INV                PIC  9(08).

       01  WRK-DATA-DB2.
           05  WRK-DIA                 PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '.'.
           05  WRK-MES                 PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '.'.
           05  WRK-ANO                 PIC  9(04)          VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** CHAVES DE COMPARACAO ***'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-AGENDOLD.
           05  WRK-CH-OLD-AGENCIA     PIC  9(05)          VALUE ZEROS.
           05  WRK-CH-OLD-CONTA       PIC  9(07)          VALUE ZEROS.
           05  WRK-CH-OLD-PROD        PIC  9(02)          VALUE ZEROS.
           05  WRK-CH-OLD-DATA        PIC  9(08)          VALUE ZEROS.

       01  WRK-CHAVE-AGENDNEW.
           05  WRK-CH-NEW-AGENCIA     PIC  9(05)          VALUE ZEROS.
           05  WRK-CH-NEW-CONTA       PIC  9(07)          VALUE ZEROS.
           05  WRK-CH-NEW-PROD        PIC  9(02)          VALUE ZEROS.
           05  WRK-CH-NEW-DATA        PIC  9(08)          VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DT-JULIANA          PIC  9(05) COMP-3   VALUE ZEROS.
           05  WRK-DT-AAMMDD           PIC  9(07) COMP-3   VALUE ZEROS.
           05  WRK-DT-AAAAMMDD         PIC  9(09) COMP-3   VALUE ZEROS.
           05  WRK-TI-HHMMSS           PIC  9(07) COMP-3   VALUE ZEROS.
           05  WRK-TI-HHMMSSMMM        PIC  9(13) COMP-3   VALUE ZEROS.
           05  WRK-TIMESTAMP.
               07 WRK-SEC-ANO-SISTEMA  PIC  9(04)          VALUE ZEROS.
               07 WRK-MES-SISTEMA      PIC  9(02)          VALUE ZEROS.
               07 WRK-DIA-SISTEMA      PIC  9(02)          VALUE ZEROS.
               07 WRK-HH-SISTEMA       PIC  9(02)          VALUE ZEROS.
               07 WRK-MM-SISTEMA       PIC  9(02)          VALUE ZEROS.
               07 WRK-SS-SISTEMA       PIC  9(02)          VALUE ZEROS.
               07 FILLER               PIC  9(06)          VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** LAYOUT DO ARQUIVO RELATUAL ***'.
      *----------------------------------------------------------------*

       01  WRK-CABEC1.
           03  WRK-CB1-CARRO           PIC X(001) VALUE '1'.
           03  WRK-CB1-DATA.
               05  WRK-CB1-DIA         PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE '/'.
               05  WRK-CB1-MES         PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE '/'.
               05  WRK-CB1-ANO         PIC 9(004) VALUE ZEROS.
           03  FILLER                  PIC X(040) VALUE SPACES.
           03  FILLER                  PIC X(066) VALUE
              'BRADESCO CARTOES'.
           03  FILLER                  PIC X(008) VALUE
              'PAGINA: '.
           03  WRK-CB1-PAGINA          PIC ZZZ9   VALUE ZEROS.

       01  WRK-CABEC2.
           03  WRK-CB2-CARRO           PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(008) VALUE 'BVVE0551'.
           03  FILLER                  PIC X(036) VALUE SPACES.
           03  FILLER                  PIC X(055) VALUE
              'TRAG DA AGENDA FINANCEIRA'.
           03  FILLER                  PIC X(021) VALUE SPACES.
           03  WRK-CB2-HORAS.
               05  WRK-CB2-HORA        PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  WRK-CB2-MINUTO      PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  WRK-CB2-SEGUNDO     PIC 9(002) VALUE ZEROS.

       01  WRK-CABEC3.
           03  WRK-CB3-CARRO           PIC X(001) VALUE '0'.
           03  FILLER                  PIC X(041) VALUE SPACES.
           03  FILLER                  PIC X(130) VALUE
               'CONTAS DESTINO PARA ATUALIZACAO'.

       01  WRK-CABEC5.
           03  WRK-CB5-CARRO           PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(002) VALUE SPACES.
           03  FILLER                  PIC X(130) VALUE
               'AGENCIA   CONTA     PRODUTO    DATA        VLR CREDITO
      -        '    VLR ANTEC '.

       01  WRK-LINDET1.
           03  WRK-LD1-CARRO       PIC X(001)         VALUE SPACE.
           03  FILLER              PIC X(003)         VALUE SPACES.
           03  WRK-LD1-AGENCIA     PIC  ZZ999         VALUE ZEROS.
           03  FILLER              PIC X(002)         VALUE SPACES.
           03  WRK-LD1-CONTA       PIC ZZZZ999        VALUE SPACES.
           03  FILLER              PIC X(007)         VALUE SPACES.
           03  WRK-LD1-COD-PRODUTO PIC 99             VALUE ZEROS.
           03  FILLER              PIC X(005)         VALUE SPACES.
           03  WRK-LD1-DATA        PIC X(010)         VALUE ZEROS.
           03  FILLER              PIC X(002)         VALUE SPACES.
           03  WRK-LD1-VLR-CRED    PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           03  FILLER              PIC X(002)         VALUE SPACES.
           03  WRK-LD1-VLR-ATUAL   PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.

       01  WRK-LINTOT1    .
           03  WRK-LT1-CARRO            PIC X(001) VALUE '0'.
           03  FILLER                   PIC X(029) VALUE
               'TOTAL MOVIMENTOS......: '.
           03  WRK-LT1-TOTAL            PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** FIM DA WORKING-STORAGE ***'.
      *----------------------------------------------------------------*

      ******************************************************************
       PROCEDURE                       DIVISION.
      ******************************************************************

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM  1000-INICIO
           PERFORM  5000-PROCESSO   UNTIL  WRK-FS-AGENDOLD  EQUAL  '10'
           PERFORM  8000-FINALIZA

           STOP RUN.

      *----------------------------------------------------------------*
       0000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIO                     SECTION.
      *----------------------------------------------------------------*

           OPEN  INPUT  AGENDOLD
                        AGENDNEW
                 OUTPUT AGENDATU
                        RELATUAL

           MOVE  WRK-ABERTURA                   TO  WRK-OPERACAO

           PERFORM  1100-TESTAR-FILE-STATUS
           PERFORM  1200-TRATAR-PRIMEIRA-LEITURA.

           CALL 'POOL7600'             USING WRK-DATA-HORA.
           MOVE  WRK-DIA-SISTEMA       TO WRK-CB1-DIA
           MOVE  WRK-MES-SISTEMA       TO WRK-CB1-MES
           MOVE  WRK-SEC-ANO-SISTEMA   TO WRK-CB1-ANO
           MOVE  WRK-HH-SISTEMA        TO WRK-CB2-HORA
           MOVE  WRK-MM-SISTEMA        TO WRK-CB2-MINUTO
           MOVE  WRK-SS-SISTEMA        TO WRK-CB2-SEGUNDO.

      *----------------------------------------------------------------*
       1000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM  1110-TESTAR-FS-AGENDOLD
           PERFORM  1120-TESTAR-FS-AGENDNEW
           PERFORM  1130-TESTAR-FS-AGENDATU
           PERFORM  1150-TESTAR-FS-RELATUAL.

      *----------------------------------------------------------------*
       1100-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-AGENDOLD         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AGENDOLD        NOT  EQUAL '00'
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE 'AGENDOLD'         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-AGENDOLD    TO  WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-AGENDNEW         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AGENDNEW        NOT  EQUAL '00'
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE 'AGENDNEW'         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-AGENDNEW    TO  WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-AGENDATU         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AGENDATU        NOT  EQUAL '00'
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE 'AGENDATU'         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-AGENDATU    TO  WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-TESTAR-FS-RELATUAL         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATUAL        NOT  EQUAL '00'
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE 'RELATUAL'         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-RELATUAL    TO  WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-TRATAR-PRIMEIRA-LEITURA    SECTION.
      *----------------------------------------------------------------*

           PERFORM  1210-LER-AGENDOLD

           IF  WRK-FS-AGENDOLD      EQUAL  '10'   AND
               ACU-LIDOS-AGENDOLD   EQUAL   ZEROS
               MOVE  'AGENDOLD'        TO  WRK-ARQ-VAZIO
               PERFORM  1298-DISPLAY-ARQ-VAZIO
           END-IF.

           PERFORM  1220-LER-AGENDNEW

           IF  WRK-FS-AGENDNEW      EQUAL  '10'   AND
               ACU-LIDOS-AGENDNEW   EQUAL   ZEROS
               MOVE  'AGENDNEW'        TO  WRK-ARQ-VAZIO
               PERFORM  1298-DISPLAY-ARQ-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-LER-AGENDOLD             SECTION.
      *----------------------------------------------------------------*

           READ  AGENDOLD            INTO  WRK-AGENDOLD

           IF  WRK-FS-AGENDOLD       EQUAL  '10'
               MOVE  HIGH-VALUE      TO  WRK-CHAVE-AGENDOLD
               GO TO 1210-99-FIM
           END-IF.

           MOVE  WRK-LEITURA         TO  WRK-OPERACAO
           PERFORM  1110-TESTAR-FS-AGENDOLD

           ADD   1                   TO  ACU-LIDOS-AGENDOLD

           MOVE  WRK-OLD-AGENCNEW      TO  WRK-CH-OLD-AGENCIA
           MOVE  WRK-OLD-CONTANEW      TO  WRK-CH-OLD-CONTA
           MOVE  WRK-OLD-CIDTFD-PRODT-CATAO
                                       TO  WRK-SINALIZADO
           MOVE  WRK-9-2               TO  WRK-CH-OLD-PROD.
           MOVE  WRK-OLD-DMOVTO        TO  WRK-DATA-DB2
           MOVE  CORR WRK-DATA-DB2     TO  WRK-DATA-INV
           MOVE  WRK-DATA-INV-R        TO  WRK-CH-OLD-DATA.

      *----------------------------------------------------------------*
       1210-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1220-LER-AGENDNEW               SECTION.
      *----------------------------------------------------------------*

           READ  AGENDNEW            INTO  WRK-AGENDNEW

           IF  WRK-FS-AGENDNEW  EQUAL  '10'
               MOVE  HIGH-VALUE         TO  WRK-CHAVE-AGENDNEW
               GO TO 1220-99-FIM
           END-IF.

           MOVE  WRK-LEITURA         TO  WRK-OPERACAO
           PERFORM  1120-TESTAR-FS-AGENDNEW
           ADD   1                   TO  ACU-LIDOS-AGENDNEW

           MOVE  WRK-NEW-CJUNC-DEPDC  TO  WRK-SINALIZADO
           MOVE  WRK-9-5              TO  WRK-CH-NEW-AGENCIA
           MOVE  WRK-NEW-CCTA-CORR    TO  WRK-SINALIZADO
           MOVE  WRK-9-7              TO  WRK-CH-NEW-CONTA.
           MOVE  WRK-NEW-CIDTFD-PRODT-CATAO
                                      TO  WRK-SINALIZADO
           MOVE  WRK-9-2              TO  WRK-CH-NEW-PROD.
           MOVE  WRK-NEW-DMOVTO       TO  WRK-DATA-DB2
           MOVE  CORR WRK-DATA-DB2    TO  WRK-DATA-INV
           MOVE  WRK-DATA-INV-R       TO  WRK-CH-NEW-DATA.

      *----------------------------------------------------------------*
       1220-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1298-DISPLAY-ARQ-VAZIO          SECTION.
      *----------------------------------------------------------------*

           DISPLAY '****************** BVVE0551 ******************'
           DISPLAY '*                                            *'
           DISPLAY '*        ARQUIVO ' WRK-ARQ-VAZIO
                                            ' ESTA VAZIO         *'
           DISPLAY '*                                            *'
           DISPLAY '****************** BVVE0551 ******************'.

      *----------------------------------------------------------------*
       1298-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-PROCESSO                   SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-AGENDOLD      EQUAL  WRK-CHAVE-AGENDNEW
               PERFORM  5100-TRATAR-IGUAL
               PERFORM  1220-LER-AGENDNEW
               PERFORM  1210-LER-AGENDOLD
           ELSE
               IF  WRK-CHAVE-AGENDOLD      LESS WRK-CHAVE-AGENDNEW
                   PERFORM  5200-TRATAR-MENOR
                   PERFORM  1210-LER-AGENDOLD
               ELSE
                   PERFORM  1220-LER-AGENDNEW
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5100-TRATAR-IGUAL               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AGENDATU

           MOVE WRK-NEW-CJUNC-DEPDC        TO WRK-ATU-CJUNC-DEPDC
           MOVE WRK-NEW-CCTA-CORR          TO WRK-ATU-CCTA-CORR
           MOVE WRK-NEW-CIDTFD-PRODT-CATAO TO WRK-ATU-CIDTFD-PRODT-CATAO
           MOVE WRK-NEW-DMOVTO             TO WRK-ATU-DMOVTO

           COMPUTE WRK-ATU-VMOVTO-DEB-CATAO = WRK-NEW-VMOVTO-DEB-CATAO +
                                              WRK-OLD-VMOVTO-DEB-CATAO

           MOVE 'A'                        TO WRK-ATU-MANUTENCAO
           PERFORM 5110-GRAVAR-AGENDATU.

           MOVE WRK-NEW-VMOVTO-CREDT-CATAO TO WRK-13-SINALIZADO
           MOVE WRK-9-9V99                 TO WRK-LD1-VLR-CRED
           MOVE WRK-ATU-VMOVTO-DEB-CATAO   TO WRK-13-SINALIZADO
           MOVE WRK-9-9V99                 TO WRK-LD1-VLR-ATUAL

           PERFORM 5300-GRAVAR-RELATUAL.

           INITIALIZE WRK-AGENDATU

           MOVE WRK-OLD-CJUNC-DEPDC        TO WRK-ATU-CJUNC-DEPDC
           MOVE WRK-OLD-CCTA-CORR          TO WRK-ATU-CCTA-CORR
           MOVE WRK-OLD-CIDTFD-PRODT-CATAO TO WRK-ATU-CIDTFD-PRODT-CATAO
           MOVE WRK-OLD-DMOVTO             TO WRK-ATU-DMOVTO
           MOVE 'D'                        TO WRK-ATU-MANUTENCAO
           PERFORM  5110-GRAVAR-AGENDATU.

      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5110-GRAVAR-AGENDATU          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO         TO  WRK-OPERACAO.

           WRITE  FD-REG-AGENDATU    FROM  WRK-AGENDATU
           PERFORM 1130-TESTAR-FS-AGENDATU

           ADD    1                    TO  ACU-GRAVA-AGENDATU.

      *----------------------------------------------------------------*
       5110-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5200-TRATAR-MENOR               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AGENDATU

           MOVE WRK-OLD-AGENCNEW           TO WRK-ATU-CJUNC-DEPDC
           MOVE WRK-OLD-CONTANEW           TO WRK-ATU-CCTA-CORR
           MOVE WRK-OLD-CIDTFD-PRODT-CATAO TO WRK-ATU-CIDTFD-PRODT-CATAO
           MOVE WRK-OLD-DMOVTO             TO WRK-ATU-DMOVTO
           MOVE WRK-OLD-VMOVTO-DEB-CATAO   TO WRK-ATU-VMOVTO-DEB-CATAO
           MOVE 'I'                        TO WRK-ATU-MANUTENCAO

           PERFORM 5110-GRAVAR-AGENDATU.

           MOVE ZEROS                      TO WRK-LD1-VLR-CRED
           MOVE WRK-ATU-VMOVTO-DEB-CATAO   TO WRK-13-SINALIZADO
           MOVE WRK-9-9V99                 TO WRK-LD1-VLR-ATUAL

           PERFORM 5300-GRAVAR-RELATUAL.

           INITIALIZE WRK-AGENDATU

           MOVE WRK-OLD-CJUNC-DEPDC        TO WRK-ATU-CJUNC-DEPDC
           MOVE WRK-OLD-CCTA-CORR          TO WRK-ATU-CCTA-CORR
           MOVE WRK-OLD-CIDTFD-PRODT-CATAO TO WRK-ATU-CIDTFD-PRODT-CATAO
           MOVE WRK-OLD-DMOVTO             TO WRK-ATU-DMOVTO
           MOVE 'D'                        TO WRK-ATU-MANUTENCAO

           PERFORM  5110-GRAVAR-AGENDATU.

      *----------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5300-GRAVAR-RELATUAL            SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS  GREATER  56
               PERFORM  5310-IMPRIMIR-CABECS
           END-IF.

           MOVE WRK-ATU-CJUNC-DEPDC        TO WRK-SINALIZADO
           MOVE WRK-9-5                    TO WRK-LD1-AGENCIA
           MOVE WRK-ATU-CCTA-CORR          TO WRK-SINALIZADO
           MOVE WRK-9-7                    TO WRK-LD1-CONTA
           MOVE WRK-ATU-CIDTFD-PRODT-CATAO TO WRK-SINALIZADO
           MOVE WRK-9-2                    TO WRK-LD1-COD-PRODUTO
           MOVE WRK-ATU-DMOVTO             TO WRK-LD1-DATA

           WRITE  FD-REG-RELATUAL    FROM  WRK-LINDET1
           PERFORM  1150-TESTAR-FS-RELATUAL

           ADD    1                    TO  ACU-LINHAS
                                           ACU-GRAVA-RELATUAL.

      *----------------------------------------------------------------*
       5300-99-FIM.                     EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5310-IMPRIMIR-CABECS            SECTION.
      *----------------------------------------------------------------*

           ADD   1                     TO  ACU-PAGINAS
           MOVE  ACU-PAGINAS           TO  WRK-CB1-PAGINA
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO

           WRITE  FD-REG-RELATUAL    FROM  WRK-CABEC1
           PERFORM  1150-TESTAR-FS-RELATUAL

           WRITE  FD-REG-RELATUAL    FROM  WRK-CABEC2
           PERFORM  1150-TESTAR-FS-RELATUAL

           WRITE  FD-REG-RELATUAL    FROM  WRK-CABEC3
           PERFORM  1150-TESTAR-FS-RELATUAL

           WRITE  FD-REG-RELATUAL    FROM  WRK-CABEC5
           PERFORM  1150-TESTAR-FS-RELATUAL

           MOVE  6                     TO  ACU-LINHAS.

      *----------------------------------------------------------------*
       5310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------
       8000-FINALIZA                   SECTION.
      *----------------------------------------------------------------*

           PERFORM  8100-IMPRIMIR-TOTAL.

           DISPLAY '****************** BVVE0551 ******************'
           DISPLAY '*                                            *'
           DISPLAY '*            FIM DE PROCESSAMENTO            *'
           DISPLAY '*                                            *'
           DISPLAY '*                                            *'
           MOVE  ACU-LIDOS-AGENDOLD    TO  WRK-MASC
           DISPLAY '*  TOTAL LIDOS AGENDOLD       = ' WRK-MASC
                                                              '  *'
           MOVE  ACU-LIDOS-AGENDNEW    TO  WRK-MASC
           DISPLAY '*  TOTAL LIDOS AGENDNEW       = ' WRK-MASC
                                                              '  *'
           DISPLAY '*                                            *'
           MOVE  ACU-GRAVA-AGENDATU    TO  WRK-MASC
           DISPLAY '*  TOTAL GRAVADOS AGENDATU    = ' WRK-MASC
                                                              '  *'
           MOVE  ACU-GRAVA-RELATUAL    TO  WRK-MASC
           DISPLAY '*  TOTAL GRAVADOS RELATUAL    = ' WRK-MASC
                                                              '  *'
           DISPLAY '*                                            *'
           DISPLAY '****************** BVVE0551 ******************'

           CLOSE  AGENDOLD
                  AGENDNEW
                  AGENDATU
                  RELATUAL.

           MOVE  WRK-FECHAMENTO          TO  WRK-OPERACAO

           PERFORM  1100-TESTAR-FILE-STATUS
           GOBACK.

      *----------------------------------------------------------------*
       8000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8100-IMPRIMIR-TOTAL             SECTION.
      *----------------------------------------------------------------*

           MOVE  ACU-GRAVA-RELATUAL    TO  WRK-LT1-TOTAL

           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO

           WRITE  FD-REG-RELATUAL    FROM  WRK-LINTOT1
           PERFORM  1150-TESTAR-FS-RELATUAL.

      *---------------------------------------------------------------*
       8100-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE0551'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.


