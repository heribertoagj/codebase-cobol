      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE0537.
       AUTHOR.     FRANCISMARA.
      *================================================================*
      *                    B S I   T E C N O L O G I A                 *
      *----------------------------------------------------------------*
      *   ANALISTA....: FRANCISMARA         -  BSI TECNOLOGIA          *
      *   DATA........: JANEIRO/2017                                   *
      *----------------------------------------------------------------*
      *   OBJETIVO....: LISTAR AS SOLICITACOES DE ALTERACAO DE TRAVA   *
      *                 BATCH E ONLINE                                 *
      *                 SELECIONA AS ALTERACOES EFETUADAS NA DATA DE   *
      *                 MOVIMENTO E Q TENHA SIDO ALTERADA AG E CTA     *
      *----------------------------------------------------------------*
      *   ARQUIVOS....: DDNAME                      INCLUDE/BOOK       *
      *                ALTETRAV (I)                   I#BVVED3         *
      *                RELATO (O)                       -              *
      *                ALTESELE (O)                     -              *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - AREA PARA A BRAD7100                             *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - MODULO PARA CANCELAMENTO DO PROGRAMA             *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ALTESELE ASSIGN      TO UT-S-ALTESELE
                    FILE STATUS        IS WRK-FS-ALTESELE.

           SELECT ALTETRAV  ASSIGN     TO UT-S-ALTETRAV
                      FILE STATUS      IS WRK-FS-ALTETRAV.

           SELECT RELATO  ASSIGN       TO UT-S-RELATO
                    FILE STATUS        IS WRK-FS-RELATO.

           SELECT TRAVPBAN     ASSIGN  TO  TRAVPBAN
                  ORGANIZATION         IS  INDEXED
                  ACCESS MODE          IS  DYNAMIC
                  RECORD KEY           IS  BVVED1-CHAVE
                  FILE STATUS          IS  WRK-FS-TRAVPBAN.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE MANUTENCOES                              *
      *            ORG. SEQUENCIAL - LRECL = 600                       *
      *----------------------------------------------------------------*

       FD  ALTETRAV
           LABEL RECORD STANDARD
           RECORDING F
           BLOCK 0.

       01  FD-REG-ALTETRAV             PIC  X(600).

      *----------------------------------------------------------------*
      *    OUTPUT- ALTERACOES SELECIONADAS                             *
      *            ORG. SEQUENCIAL - LRECL = 50                        *
      *----------------------------------------------------------------*

       FD  ALTESELE
           RECORDING  MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ALTESELE.
           05  FILLER               PIC  X(050).


      *----------------------------------------------------------------*
      *    OUTPUT- RELATORIO DE MANUTENCOES CONSISTENTES               *
      *            ORG. FIXO - LRECL = 133                             *
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELATO             PIC  X(133).

      *----------------------------------------------------------------*
      * ARQVUIVO DE VSAM ES/CREDENCIADORA       - SAIDA   LRECL = 30  *
      *----------------------------------------------------------------*

       FD  TRAVPBAN.

       COPY 'I#BVVED1'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVE0537 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-MULADORES.
           03  ACU-GRA-ALTESELE        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LID-ALTETRAV        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-RELATO          PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-SELECIONADOS        PIC  9(09) COMP-3   VALUE ZEROS.

       77  ACU-LINHAS                  PIC  9(007) COMP-3  VALUE 99.
       77  ACU-PAGINAS                 PIC  9(004) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE OPERACAO DE ARQUIVOS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-ALTESELE             PIC X(02)           VALUE SPACES.
       77  WRK-FS-ALTETRAV             PIC X(02)           VALUE SPACES.
       77  WRK-FS-RELATO               PIC X(02)           VALUE SPACES.
       77  WRK-FS-TRAVPBAN             PIC  X(02)          VALUE SPACES.

       01  WRK-ABERTURA                PIC X(13)  VALUE ' NA ABERTURA '.
       01  WRK-GRAVACAO                PIC X(13)  VALUE ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC X(13)  VALUE 'NO FECHAMENTO'.
       01  WRK-LEITURA                 PIC X(13)  VALUE ' NA LEITURA  '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)       VALUE 'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.
       01  WRK-IND                     PIC  9(04) COMP-3   VALUE ZEROS.

7C2511 01  WRK-CNPJ-CPF-CLI            PIC X(014)          VALUE SPACES.
7C2511 01  WRK-CNPJ-AUX REDEFINES WRK-CNPJ-CPF-CLI.
7C2511*    03  WRK-CNPJ                PIC 9(008).
7C2511     03  WRK-CNPJ                PIC X(008).
7C2511*    03  WRK-FILIAL              PIC 9(004).
7C2511     03  WRK-FILIAL              PIC X(004).
7C2511     03  WRK-CNPJ-CTRL           PIC 9(002).
7C2511 01  WRK-CPF-AUX REDEFINES WRK-CNPJ-CPF-CLI.
7C2511     03  FILLER                  PIC 9(002).
7C2511     03  WRK-CPF                 PIC 9(009).
7C2511     03  WRK-CPF-CTRL            PIC 9(002).
7C2511 01  FILLER      REDEFINES WRK-CNPJ-CPF-CLI.
7C2511     03  FILLER                  PIC 9(006).
7C2511     03  WRK-CNPJ-RAIZ           PIC X(008).

7C2511*01  WRK-CNPJ-EDIT.
7C2511*    03  WRK-CNPJ                PIC 99.999.999      VALUE ZEROS.
7C2511*    03  FILLER                  PIC X(001)          VALUE '/'.
7C2511*    03  WRK-FILIAL              PIC 9(004)          VALUE ZEROS.
7C2511*    03  FILLER                  PIC X(001)          VALUE '-'.
7C2511*    03  WRK-CNPJ-CTRL           PIC 9(002)          VALUE ZEROS.
7C2511 01  WRK-CNPJ-EDIT.
7C2511     03  WRK-CNPJ-ED-PT1         PIC X(02).
7C2511     03  FILLER                  PIC X(01)           VALUE '.'.
7C2511     03  WRK-CNPJ-ED-PT2         PIC X(03).
7C2511     03  FILLER                  PIC X(01)           VALUE '.'.
7C2511     03  WRK-CNPJ-ED-PT3         PIC X(03).
7C2511     03  FILLER                  PIC X(01)           VALUE '/'.
7C2511     03  WRK-FILIAL-ED           PIC X(04).
7C2511     03  FILLER                  PIC X(01)           VALUE '-'.
7C2511     03  WRK-CNPJ-CTRL-ED        PIC 9(02).

       01  WRK-CPF-EDIT.
           03  WRK-CPF                 PIC 999.999.999      VALUE ZEROS.
           03  FILLER                  PIC X(001)          VALUE '-'.
           03  WRK-CPF-CTRL            PIC 9(002)          VALUE ZEROS.

       01  WRK-AUX-CONTA-CIP           PIC 9(13)           VALUE ZEROS.
       01  FILLER REDEFINES WRK-AUX-CONTA-CIP .
           03  FILLER                  PIC X(05).
           03  WRK-CONTA-CIP           PIC 9(07).
           03  WRK-DIG-CIP             PIC X(01).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** TABELA BANDEIRAS  ***'.
      *----------------------------------------------------------------*

       01  TAB-TABELA-BANDEIRAS.
           03  FILLER OCCURS 999.
               05  TAB-COD-BAND        PIC  X(003)  VALUE SPACES.
               05  TAB-DESCRICAO       PIC  X(008)  VALUE SPACES.
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA MENSAGENS DE ERRO     *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-ERRO.
          03  FILLER                   PIC X(07)   VALUE SPACES.
          03  FILLER                   PIC X(07)   VALUE
              '* ERRO '.
          03  WRK-OPERACAO             PIC X(13)   VALUE SPACES.
          03  FILLER                   PIC X(12)   VALUE
              ' DO ARQUIVO '.
          03  WRK-NOME-ARQ             PIC X(08)   VALUE SPACES.
          03  FILLER                   PIC X(17)   VALUE
              ' - FILE-STATUS = '.
          03  WRK-FILE-STATUS          PIC X(02)   VALUE SPACES.
          03  FILLER                   PIC X(02)   VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           '*** AREA DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01    WRK-DATA-HORA.
         03  WRK-DT-JULIANA            PIC  9(05)  COMP-3 VALUE ZEROS.
         03  WRK-DT-AAMMDD             PIC  9(07)  COMP-3 VALUE ZEROS.
         03  WRK-DT-AAAAMMDD           PIC  9(09)  COMP-3 VALUE ZEROS.
         03  WRK-TI-HHMMSS             PIC  9(07)  COMP-3 VALUE ZEROS.
         03  WRK-TI-HHMMSSMMMMMM       PIC  9(13)  COMP-3 VALUE ZEROS.
         03  WRK-TIMESTAMP.
           05  WRK-DATA-ATUAL          PIC  9(08)   VALUE ZEROS.
           05  FILLER REDEFINES WRK-DATA-ATUAL.
               10 WRK-TIMESTAMP-AAAA   PIC  9(04).
               10 WRK-TIMESTAMP-MM     PIC  9(02).
               10 WRK-TIMESTAMP-DD     PIC  9(02).
           05 WRK-TIMESTAMP-HH         PIC  9(02)   VALUE ZEROS.
           05 WRK-TIMESTAMP-MN         PIC  9(02)   VALUE ZEROS.
           05 WRK-TIMESTAMP-SS         PIC  9(02)   VALUE ZEROS.
           05 WRK-TIMESTAMP-MIL        PIC  9(06)   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)         VALUE
           '* AREA DO ARQUIVO ALTESELE'.
      *----------------------------------------------------------------*

       01  WRK-REG-ALTESELE.
           05  WRK-SEL-AGENCOLD        PIC  9(05)          VALUE ZEROS.
           05  WRK-SEL-CONTAOLD        PIC  9(07)          VALUE ZEROS.
           05  WRK-SEL-DGCTAOLD        PIC  X(01)          VALUE SPACES.
           05  WRK-SEL-AGENCNEW        PIC  9(05)          VALUE ZEROS.
           05  WRK-SEL-CONTANEW        PIC  9(07)          VALUE ZEROS.
           05  WRK-SEL-DGCTANEW        PIC  X(01)          VALUE SPACES.
           05  WRK-SEL-BANDEIRA        PIC  X(03)          VALUE SPACES.
           05  FILLER                  PIC  X(21)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)         VALUE
           '* AREA DO ARQUIVO ALTETRAV *'.
      *----------------------------------------------------------------*
       COPY 'I#BVVED3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*** AREA DO RELINCO ***'.
      *----------------------------------------------------------------*
       01  WRK-CABEC1.
           03  CB1-CARRO               PIC X(001) VALUE '1'.
           03  CB1-DATA.
               05  CB1-DIA             PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE '/'.
               05  CB1-MES             PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE '/'.
               05  CB1-ANO             PIC 9(004) VALUE ZEROS.
           03  FILLER                  PIC X(035) VALUE SPACES.
           03  FILLER                  PIC X(046) VALUE
              'BRADESCO CARTOES'.
           03  FILLER                  PIC X(008) VALUE
              'PAGINA: '.
           03  CB1-PAGINA              PIC ZZZ9   VALUE ZEROS.

       01  WRK-CABEC2.
           03  CB2-CARRO               PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(008) VALUE 'BVVE0537'.
           03  FILLER                  PIC X(087) VALUE
              '                  ALTERACOES DE TRAVA SELECIONADAS PARA T
      -       'RAG NA AGENDA                 '.
           03  CB2-HORAS.
               05  CB2-HORA            PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  CB2-MINUTO          PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  CB2-SEGUNDO         PIC 9(002) VALUE ZEROS.

       01  WRK-CABEC3.
           03  CB3-CARRO                PIC X(001) VALUE '0'.
           03  FILLER                   PIC X(003) VALUE SPACES.
           03  FILLER                   PIC X(090) VALUE
               '!....ANTIGA.....!    !......NOVA.....!'.

       01  WRK-CABEC4.
           03  CB4-CARRO                PIC X(001) VALUE ' '.
           03  FILLER                   PIC X(004) VALUE SPACES.
           03  FILLER                   PIC X(104) VALUE
               'AGENCIA   CONTA      AGENCIA   CONTA         CNPJ/CPF
      -        '    BAND   RAIZ    NUM IDENT. SCG   USUARIO'.

       01  WRK-LINDET1    .
           03  LD1-CARRO                PIC X(001)     VALUE SPACE.
           03  FILLER                   PIC X(003)     VALUE SPACE.
           03  LD1-AGE-OLD              PIC ZZZZ9      VALUE ZEROS.
           03  FILLER                   PIC X(002)     VALUE SPACE.
           03  LD1-CTA-OLD              PIC ZZZZZZ9    VALUE ZEROS.
           03  FILLER                   PIC X(006)     VALUE SPACE.
           03  LD1-AGE-NEW              PIC ZZZZ9      VALUE ZEROS.
           03  FILLER                   PIC X(003)     VALUE SPACE.
           03  LD1-CTA-NEW              PIC ZZZZZZ9    VALUE ZEROS.
           03  FILLER                   PIC X(004)     VALUE SPACE.
           03  LD1-CNPJ-CPF             PIC X(018)     VALUE SPACES.
           03  FILLER                   PIC X(003)     VALUE SPACES.
           03  LD1-BAND                 PIC X(008)     VALUE SPACES.
           03  FILLER                   PIC X(001)     VALUE SPACES.
           03  LD1-RAIZ                 PIC X(003)     VALUE SPACES.
           03  FILLER                   PIC X(002)     VALUE SPACES.
           03  LD1-NUM-SCG              PIC 9(017)     VALUE ZEROS.
           03  FILLER                   PIC X(002)     VALUE SPACES.
           03  LD1-USUARIO              PIC X(007)     VALUE SPACES.

       01  WRK-LINTOT3    .
           03  WRK-LT3-CARRO            PIC X(001) VALUE ' '.
           03  FILLER                   PIC X(030) VALUE
               'TOTAL REGISTROS ............: '.
           03  WRK-LT3-TOTAL            PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)          VALUE
           '*  AREA DA POOL7100  *'.
      *----------------------------------------------------------------*
       COPY  'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM DA WORKING BVVE0537    *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA.

           PERFORM 2000-TESTAR-VAZIO.

           PERFORM 3000-PROCESSAR  UNTIL WRK-FS-ALTETRAV
                                   EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT ALTETRAV
                      TRAVPBAN
               OUTPUT  RELATO
                       ALTESELE.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           PERFORM 1400-OBTER-DATA-CORRENTE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1220-TESTAR-FS-ALTESELE.

           PERFORM 1230-TESTAR-FS-TRAVPBAN.

           PERFORM 1250-TESTAR-FS-ALTETRAV.

           PERFORM 1260-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1220-TESTAR-FS-ALTESELE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ALTESELE         NOT EQUAL '00'
               MOVE 'ALTESELE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ALTESELE    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1230-TESTAR-FS-TRAVPBAN         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-TRAVPBAN         NOT EQUAL  '00'  AND '10'
                                           AND    '23'
               MOVE  'APL'             TO  ERR-TIPO-ACESSO
               MOVE  'TRAVPBAN'        TO  WRK-NOME-ARQ
               MOVE  WRK-FS-TRAVPBAN   TO  WRK-FILE-STATUS
               MOVE  WRK-MSG-ERRO  TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1230-99-EXIT.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1250-TESTAR-FS-ALTETRAV          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ALTETRAV          NOT EQUAL '00'
               MOVE 'ALTETRAV'          TO WRK-NOME-ARQ
               MOVE WRK-FS-ALTETRAV     TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1260-TESTAR-FS-RELATO          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATO          NOT EQUAL '00'
               MOVE 'RELATO'          TO WRK-NOME-ARQ
               MOVE WRK-FS-RELATO     TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-OBTER-DATA-CORRENTE        SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE  WRK-TIMESTAMP-AAAA    TO  CB1-ANO
           MOVE  WRK-TIMESTAMP-MM      TO  CB1-MES
           MOVE  WRK-TIMESTAMP-DD      TO  CB1-DIA
           MOVE  WRK-TIMESTAMP-HH      TO  CB2-HORA
           MOVE  WRK-TIMESTAMP-MN      TO  CB2-MINUTO
           MOVE  WRK-TIMESTAMP-SS      TO  CB2-SEGUNDO.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-TESTAR-VAZIO               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2200-LER-ALTETRAV.

           IF  WRK-FS-ALTETRAV          EQUAL '10'
               DISPLAY '****************BVVE0537******************'
               DISPLAY '*                                        *'
               DISPLAY '*          ARQ ALTETRAV VAZIO            *'
               DISPLAY '*                                        *'
               DISPLAY '****************BVVE0537******************'
           END-IF.

           PERFORM 2300-START-TRAVPBAN.

           IF  WRK-FS-TRAVPBAN         EQUAL '10'
               DISPLAY '****************BVVE0544******************'
               DISPLAY '*                                        *'
               DISPLAY '*          ARQ TRAVPBAN VAZIO            *'
               DISPLAY '*                                        *'
               DISPLAY '****************BVVE0544******************'
           ELSE
               PERFORM 2420-LER-TRAVPBAN
               PERFORM 2410-CARREGAR-TABELA
                      UNTIL WRK-FS-TRAVPBAN EQUAL '10' OR
                            WRK-IND    GREATER 999
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-ALTETRAV                SECTION.
      *----------------------------------------------------------------*

           READ ALTETRAV                INTO BVVED3-TRAVPMAN.

           IF  WRK-FS-ALTETRAV          EQUAL  '10'
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1250-TESTAR-FS-ALTETRAV.

           ADD 1                       TO ACU-LID-ALTETRAV.

           IF (BVVED3-DT-MOVTO         EQUAL WRK-DATA-ATUAL)    AND
              (BVVED3-ERRO             EQUAL ZEROS)             AND

            ( (BVVED3-AGENCIA-OLD      IS NUMERIC AND
               BVVED3-AGENCIA-OLD(2:4) NOT EQUAL BVVED3-AG-CLI)  OR

              (BVVED3-CONTA-OLD        IS NUMERIC AND
               BVVED3-CONTA-OLD        NOT EQUAL BVVED3-CT-CLI) )
               ADD 1                   TO ACU-SELECIONADOS
           ELSE
               GO TO  2200-LER-ALTETRAV
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-START-TRAVPBAN               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVED1-TRAVPBAN

           START   TRAVPBAN
                   KEY                 GREATER BVVED1-CHAVE

           IF  WRK-FS-TRAVPBAN  EQUAL  '10'
               CONTINUE
           ELSE
               MOVE  WRK-LEITURA       TO  WRK-OPERACAO
               PERFORM  1230-TESTAR-FS-TRAVPBAN
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-CARREGAR-TABELA            SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO WRK-IND.

           MOVE BVVED1-DESC-BAND(1:8)    TO TAB-DESCRICAO(WRK-IND).
           MOVE BVVED1-COD-PROP-ESQ-X    TO TAB-COD-BAND(WRK-IND).

           PERFORM 2420-LER-TRAVPBAN.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2420-LER-TRAVPBAN               SECTION.
      *----------------------------------------------------------------*

           READ    TRAVPBAN NEXT

           IF  WRK-FS-TRAVPBAN  EQUAL  '10'
               CONTINUE
           ELSE
               MOVE  WRK-LEITURA       TO  WRK-OPERACAO
               PERFORM  1230-TESTAR-FS-TRAVPBAN
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-REG-ALTESELE.

           MOVE BVVED3-AGENCIA-OLD     TO WRK-SEL-AGENCOLD
           MOVE BVVED3-CONTA-OLD       TO WRK-AUX-CONTA-CIP
           MOVE WRK-CONTA-CIP          TO WRK-SEL-CONTAOLD
           MOVE WRK-DIG-CIP            TO WRK-SEL-DGCTAOLD
           MOVE BVVED3-AG-CLI          TO WRK-SEL-AGENCNEW
           MOVE BVVED3-CT-CLI          TO WRK-AUX-CONTA-CIP
           MOVE WRK-CONTA-CIP          TO WRK-SEL-CONTANEW
           MOVE WRK-DIG-CIP            TO WRK-SEL-DGCTANEW
           MOVE BVVED3-COD-PROPT-ESQM  TO WRK-SEL-BANDEIRA.

           MOVE  WRK-GRAVACAO          TO WRK-OPERACAO
           WRITE FD-REG-ALTESELE     FROM WRK-REG-ALTESELE.
           PERFORM 1260-TESTAR-FS-RELATO
           ADD  1                       TO ACU-GRA-ALTESELE.

           PERFORM 3100-EMITIR-RELATO.

           PERFORM 2200-LER-ALTETRAV.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-EMITIR-RELATO              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM VARYING             WRK-IND FROM 1 BY 1
              UNTIL               WRK-IND GREATER 999 OR
              BVVED3-COD-PROPT-ESQM  EQUAL TAB-COD-BAND(WRK-IND)
           END-PERFORM.

           IF  BVVED3-COD-PROPT-ESQM      EQUAL TAB-COD-BAND(WRK-IND)
              MOVE  TAB-DESCRICAO(WRK-IND)  TO  LD1-BAND
           ELSE
              MOVE  SPACES                  TO  LD1-BAND
           END-IF.

           IF  ACU-LINHAS              GREATER 57
               PERFORM 3110-IMPRIMIR-CABECALHO
           END-IF.

           IF  BVVED3-SOLICT-RAIZ       EQUAL 'S'
               MOVE 'SIM'              TO  LD1-RAIZ
           ELSE
               MOVE 'NAO'              TO  LD1-RAIZ
           END-IF.

           MOVE BVVED3-NUM-IDENTC-SCG  TO  LD1-NUM-SCG.

7C2511*    IF WRK-CNPJ-CPF-CLI IS NUMERIC
7C2511*       EVALUATE TRUE
7C2511*           WHEN BVVED3-TP-PESSOA-CLI = 'J'
7C2511*               MOVE BVVED3-CNPJ-CPF-CLI TO WRK-CNPJ-CPF-CLI
7C2511*               MOVE CORR WRK-CNPJ-AUX TO WRK-CNPJ-EDIT
7C2511*               MOVE WRK-CNPJ-EDIT     TO LD1-CNPJ-CPF
7C2511*           WHEN BVVED3-TP-PESSOA-CLI = 'F'
7C2511*               MOVE BVVED3-CNPJ-CPF-CLI TO WRK-CNPJ-CPF-CLI
7C2511*               MOVE CORR WRK-CPF-AUX    TO WRK-CPF-EDIT
7C2511*               MOVE WRK-CPF-EDIT        TO LD1-CNPJ-CPF
7C2511*       END-EVALUATE
7C2511*    ELSE
7C2511*       IF BVVED3-TP-PESSOA-CLI = 'R'
7C2511*          MOVE BVVED3-CNPJ-BASE-CLI TO WRK-CNPJ-CPF-CLI
7C2511*          MOVE WRK-CNPJ-RAIZ        TO LD1-CNPJ-CPF
7C2511*       END-IF
7C2511*    END-IF.
7C2511     IF (WRK-CNPJ-CPF-CLI NOT = SPACES AND LOW-VALUES) AND
7C2511        (BVVED3-TP-PESSOA-CLI = 'J' OR 'F')
7C2511         EVALUATE TRUE
7C2511            WHEN BVVED3-TP-PESSOA-CLI = 'J'
7C2511                MOVE BVVED3-CNPJ-CPF-CLI TO WRK-CNPJ-CPF-CLI
7C2511*               MOVE CORR WRK-CNPJ-AUX TO WRK-CNPJ-EDIT
7C2511                MOVE WRK-CNPJ(1:2)     TO WRK-CNPJ-ED-PT1
7C2511                MOVE WRK-CNPJ(3:3)     TO WRK-CNPJ-ED-PT2
7C2511                MOVE WRK-CNPJ(6:3)     TO WRK-CNPJ-ED-PT3
7C2511                MOVE WRK-FILIAL        TO WRK-FILIAL-ED
7C2511                MOVE WRK-CNPJ-CTRL     TO WRK-CNPJ-CTRL-ED
7C2511                MOVE WRK-CNPJ-EDIT     TO LD1-CNPJ-CPF
7C2511            WHEN BVVED3-TP-PESSOA-CLI = 'F'
7C2511                MOVE BVVED3-CNPJ-CPF-CLI TO WRK-CNPJ-CPF-CLI
7C2511                MOVE CORR WRK-CPF-AUX    TO WRK-CPF-EDIT
7C2511                MOVE WRK-CPF-EDIT        TO LD1-CNPJ-CPF
7C2511         END-EVALUATE
7C2511     ELSE
7C2511        IF BVVED3-TP-PESSOA-CLI = 'R'
7C2511           MOVE BVVED3-CNPJ-BASE-CLI TO WRK-CNPJ-CPF-CLI
7C2511           MOVE WRK-CNPJ-RAIZ        TO LD1-CNPJ-CPF
7C2511        END-IF
7C2511     END-IF.

           MOVE WRK-SEL-AGENCOLD           TO LD1-AGE-OLD
           MOVE WRK-SEL-CONTAOLD           TO LD1-CTA-OLD
           MOVE WRK-SEL-AGENCNEW           TO LD1-AGE-NEW
           MOVE WRK-SEL-CONTANEW           TO LD1-CTA-NEW
           MOVE  BVVED3-OPERADOR           TO LD1-USUARIO

           WRITE FD-REG-RELATO             FROM WRK-LINDET1.
           PERFORM 1260-TESTAR-FS-RELATO.

           ADD 1                           TO ACU-LINHAS
                                              ACU-GRA-RELATO.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3110-IMPRIMIR-CABECALHO         SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS

           MOVE ACU-PAGINAS            TO CB1-PAGINA
           WRITE FD-REG-RELATO        FROM WRK-CABEC1
           PERFORM 1260-TESTAR-FS-RELATO

           WRITE FD-REG-RELATO        FROM WRK-CABEC2
           PERFORM 1260-TESTAR-FS-RELATO

           WRITE FD-REG-RELATO        FROM WRK-CABEC3
           PERFORM 1260-TESTAR-FS-RELATO

           WRITE FD-REG-RELATO        FROM WRK-CABEC4
           PERFORM 1260-TESTAR-FS-RELATO

           MOVE 5                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE ALTESELE
                 ALTETRAV
                 TRAVPBAN
                 RELATO.

           MOVE WRK-FECHAMENTO         TO  WRK-OPERACAO.
           PERFORM 1200-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** BVVE0537 ******************'
           DISPLAY '*                                         *'

           MOVE ACU-LID-ALTETRAV        TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS ALTETRAV: 'WRK-MASCARA '      *'

           MOVE ACU-SELECIONADOS       TO WRK-MASCARA.
           DISPLAY '*   SELECIONADOS ONLINE: 'WRK-MASCARA '      *'

           MOVE ACU-GRA-ALTESELE       TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA ALTESELE: 'WRK-MASCARA '      *'

           MOVE ACU-GRA-RELATO        TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA RELATO : 'WRK-MASCARA '      *'

           DISPLAY '*                                         *'
           DISPLAY '*************** BVVE0537 ******************'.

           MOVE ACU-GRA-RELATO        TO WRK-LT3-TOTAL
           WRITE FD-REG-RELATO        FROM WRK-LINTOT3
           PERFORM 1260-TESTAR-FS-RELATO.


      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE0537'             TO ERR-PGM.

           CALL 'BRAD7100'         USING WRK-BATCH
                                         ERRO-AREA.

           GOBACK.
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
