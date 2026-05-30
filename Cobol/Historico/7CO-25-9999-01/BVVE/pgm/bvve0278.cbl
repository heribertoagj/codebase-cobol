      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE0278.
       AUTHOR.     FRANCISMARA PAGNONCELLI.
      *                                                                *
      *================================================================*
      *                  B S I  T E C N O L O G I A                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....: BVVE0278                                      *
      *    PROGRAMADORA: FRANCISMARA PAGNONCELLI  - BSI                *
      *    ANALISTA....: FRANCISMARA PAGNONCELLI  - BSI                *
      *    DATA........: JULHO/2017                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: GERAR RELATORIO DE TRANSACOES ASLC032 RECEBIDAS
      *    DO PAG-FOR                                                  *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *       REPAGFOR - ARQUIVO DE RETORNO DO PAG FOR                 *
      *       RELINCO  - RELATORIO DE INCONSISTENTES                   *
      *       RELTOTAL - RELATORIO DE TRANSACOES AGENDADAS             *
      *       RELCONS  - RELATORIO DE CONSISTENTES                     *
      *       ENVIACIP - REGISTROS PARA ENVIO A CIP                    *
      *       REL99    - INCONSISTENTES COM ERRO 99                    *
      *----------------------------------------------------------------*
      *    ALTERACOES..:                                               *
      *    DATA       AUTOR    DESCRICAO                               *
SVK   *    06/06/2018 SIVEK    IN2266976 - TRATAR NUMERO LIQUIDACAO E  *
      *                        CODIGO OCORRENCIA                       *
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

           SELECT REPAGFOR ASSIGN      TO UT-S-REPAGFOR
                    FILE STATUS        IS WRK-FS-REPAGFOR.

           SELECT RELINCO  ASSIGN      TO UT-S-RELINCO
                    FILE STATUS        IS WRK-FS-RELINCO.

           SELECT RELCONS  ASSIGN      TO UT-S-RELCONS
                    FILE STATUS        IS WRK-FS-RELCONS.

           SELECT RELTOTAL  ASSIGN     TO UT-S-RELTOTAL
                    FILE STATUS        IS WRK-FS-RELTOTAL.

           SELECT ENVIACIP ASSIGN      TO UT-S-ENVIACIP
                    FILE STATUS        IS WRK-FS-ENVIACIP.

           SELECT REL99    ASSIGN      TO UT-S-REL99
                    FILE STATUS        IS WRK-FS-REL99.


      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT - ARQUIVO REPAGFOR                                    *
      *            ORG. SEQUENCIAL - LRECL = 160                       *
      *----------------------------------------------------------------*

       FD  REPAGFOR
           RECORDING  MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-REPAGFOR            PIC  X(160).

      *----------------------------------------------------------------*
      *    OUTPUT- RELATORIO DE INCONSISTENCIAS                        *
      *            ORG. FIXO - LRECL = 133                             *
      *----------------------------------------------------------------*

       FD  RELINCO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELINCO             PIC  X(133).

      *----------------------------------------------------------------*
      *    OUTPUT- RELATORIO DE CONSISTENCIAS                          *
      *            ORG. FIXO - LRECL = 133                             *
      *----------------------------------------------------------------*

       FD  RELCONS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELCONS             PIC  X(133).

      *----------------------------------------------------------------*
      *    OUTPUT- RELATORIO DE TOTAIS DE PROCESSAMENTO                *
      *            ORG. FIXO - LRECL = 133                             *
      *----------------------------------------------------------------*

       FD  RELTOTAL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELTOTAL             PIC  X(133).

      *----------------------------------------------------------------*
      *    OUTPUT- REGISTROS PARA RETORNO A CIP                       *
      *            ORG. FIXO - LRECL = 160                             *
      *----------------------------------------------------------------*

       FD  ENVIACIP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ENVIACIP            PIC  X(160).

      *----------------------------------------------------------------*
      *    OUTPUT- RELATORIO DE ERROS 99                               *
      *            ORG. FIXO - LRECL = 133                             *
      *----------------------------------------------------------------*

       FD  REL99
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-REL99                PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVE0278 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-MULADORES.
           03  ACU-LID-REPAGFOR        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-RELINCO         PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-RELCONS         PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-ENVIACIP        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-RELTOTAL        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-TOT-RELINCO         PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-TOT-RELCONS         PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-REL99           PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LID-PGTO            PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-AGENDADOS           PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-TOT-RELINCO-99      PIC  9(09) COMP-3   VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE OPERACAO DE ARQUIVOS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-REPAGFOR             PIC X(02)           VALUE SPACES.
       77  WRK-FS-RELINCO              PIC X(02)           VALUE SPACES.
       77  WRK-FS-RELCONS              PIC X(02)           VALUE SPACES.
       77  WRK-FS-RELTOTAL             PIC X(02)           VALUE SPACES.
       77  WRK-FS-REL99                PIC X(02)           VALUE SPACES.
       77  WRK-FS-ENVIACIP             PIC X(02)           VALUE SPACES.

       01  WRK-ABERTURA                PIC X(13)           VALUE
           ' NA ABERTURA '.
       01  WRK-GRAVACAO                PIC X(13)           VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC X(13)           VALUE
           'NO FECHAMENTO'.
       01  WRK-LEITURA                 PIC X(13)           VALUE
           ' NA LEITURA  '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)      VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-BATCH                   PIC  X(008)     VALUE 'BATCH'.
       01  WRK-IND                     PIC  9(003) COMP-3  VALUE ZEROS.
       01  WRK-IND-2                   PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-TEM-99                  PIC  X(001)         VALUE SPACES.

       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
       77  ACU-LINHAS                  PIC  9(007) COMP-3  VALUE 99.
       77  ACU-PAGINAS                 PIC  9(004) COMP-3  VALUE ZEROS.
       77  ACU-LINHAS-2                PIC  9(007) COMP-3  VALUE 99.
       77  ACU-PAGINAS-2               PIC  9(004) COMP-3  VALUE ZEROS.
       77  ACU-LINHAS-3                PIC  9(007) COMP-3  VALUE 99.
       77  ACU-LINHAS-4                PIC  9(007) COMP-3  VALUE 99.
       77  ACU-PAGINAS-4               PIC  9(004) COMP-3  VALUE ZEROS.

       77  WRK-CONSIS                  PIC  X(001)         VALUE SPACES.
       77  WRK-AGENDADO                PIC  X(001)         VALUE SPACES.

       01  ACU-VALOR-INCO-99           PIC  9(015)V99 COMP-3 VALUE 0.
       01  ACU-VALOR-CON               PIC  9(015)V99 COMP-3 VALUE 0.
       01  ACU-VALOR-INCO              PIC  9(015)V99 COMP-3 VALUE 0.
       01  ACU-VALOR-AGEN              PIC  9(015)V99 COMP-3 VALUE 0.
       01  WRK-9-13-COMP               PIC  9(013)V99 COMP-3 VALUE 0.

       01  WRK-DATA-8.
           03  WRK-ANO                 PIC 9(04)       VALUE ZEROS.
           03  WRK-MES                 PIC 9(02)       VALUE ZEROS.
           03  WRK-DIA                 PIC 9(02)       VALUE ZEROS.
       01  WRK-DATA-8N                 REDEFINES
           WRK-DATA-8                  PIC 9(08).

       01  WRK-DATA-9                  PIC 9(009)      VALUE ZEROS.
       01  FILLER REDEFINES WRK-DATA-9.
           03  FILLER                  PIC X(001).
           03  WRK-DATA-ATU-8          PIC 9(008).

       01  WRK-DATA-DB2.
           03 WRK-DIA                  PIC  9(02)         VALUE ZEROS.
           03 FILLER                   PIC  X(01)         VALUE  '/'.
           03 WRK-MES                  PIC  9(02)         VALUE ZEROS.
           03 FILLER                   PIC  X(01)         VALUE  '/'.
           03 WRK-ANO                  PIC  9(04)         VALUE ZEROS.

       01  WRK-CPF-CPNJ-EDIT           PIC  X(016)         VALUE SPACES.
       01  WRK-CNPJ-EDIT REDEFINES WRK-CPF-CPNJ-EDIT.
7C2511*           03  WRK-CCNPJ               PIC  9(008).
7C2511            03  WRK-CCNPJ               PIC  X(008).
           03  WRK-BARRA               PIC  X(001).
7C2511*           03  WRK-CFLIAL              PIC  9(004).
7C2511            03  WRK-CFLIAL              PIC  X(004).
           03  WRK-TRACO-1             PIC  X(001).
7C2511*           03  WRK-CCTRL-CNPJ          PIC  9(002).
7C2511            03  WRK-CCTRL-CNPJ          PIC  X(002).
       01  WRK-CPF-EDIT  REDEFINES WRK-CPF-CPNJ-EDIT.
7C2511*           03  WRK-CCPF                PIC  9(009).
7C2511            03  WRK-CCPF                PIC  X(009).
           03  WRK-TRACO-2             PIC  X(001).
7C2511*           03  WRK-CCTRL-CPF           PIC  9(002).
7C2511            03  WRK-CCTRL-CPF           PIC  X(002).

       01  WRK-INFO-RETORNO            PIC  X(010)        VALUE SPACES.
       01  FILLER REDEFINES WRK-INFO-RETORNO.
           03 WRK-INFO-RET             PIC  X(02) OCCURS 5.

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
      *    '*** AREA DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01    WRK-DATA-HORA.
         05  WRK-DT-JULIANA            PIC  9(05)  COMP-3 VALUE ZEROS.
         05  WRK-DT-AAMMDD             PIC  9(07)  COMP-3 VALUE ZEROS.
         05  WRK-DT-AAAAMMDD           PIC  9(09)  COMP-3 VALUE ZEROS.
         05  WRK-TI-HHMMSS             PIC  9(07)  COMP-3 VALUE ZEROS.
         05  WRK-TI-HHMMSSMMMMMM       PIC  9(13)  COMP-3 VALUE ZEROS.
         05  WRK-TIMESTAMP.
           10 WRK-TIMESTAMP-AAAA       PIC  9(04)   VALUE ZEROS.
           10 WRK-TIMESTAMP-MM         PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMESTAMP-DD         PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMESTAMP-HH         PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMESTAMP-MN         PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMESTAMP-SS         PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMESTAMP-MIL        PIC  9(06)   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*** AREA DO RELATORIOS*'.
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
           03  FILLER                  PIC X(060) VALUE
              'BRADESCO CARTOES'.
           03  FILLER                  PIC X(008) VALUE
              'PAGINA: '.
           03  WRK-CB1-PAGINA          PIC ZZZ9   VALUE ZEROS.

       01  WRK-CABEC2.
           03  WRK-CB2-CARRO           PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(008) VALUE 'BVVE0278'.
           03  FILLER                  PIC X(022) VALUE SPACES.
           03  FILLER                  PIC X(063) VALUE
              'LIQUIDACAO FINANCEIRA - RETORNO PFOR CONSISTENTES - ASLC0
      -       '32'.
           03  FILLER                  PIC X(021) VALUE SPACES.
           03  WRK-CB2-HORAS.
               05  WRK-CB2-HORA        PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  WRK-CB2-MINUTO      PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  WRK-CB2-SEGUNDO     PIC 9(002) VALUE ZEROS.

       01  WRK-CABEC2-R2.
           03  WRK-CB2-CARRO           PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(008) VALUE 'BVVE0278'.
           03  FILLER                  PIC X(022) VALUE SPACES.
           03  FILLER                  PIC X(065) VALUE
              'LIQUIDACAO FINANCEIRA - RETORNO PFOR INCONSISTENTES - ASL
      -       'C032'.
           03  FILLER                  PIC X(023) VALUE SPACES.
           03  WRK-CB2-R2-HORAS.
               05  WRK-CB2-R2-HORA     PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  WRK-CB2-R2-MINUTO   PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  WRK-CB2-R2-SEGUNDO  PIC 9(002) VALUE ZEROS.

       01  WRK-CABEC3.
           03  WRK-CB3-CARRO           PIC X(001) VALUE '0'.
           03  FILLER                  PIC X(008) VALUE
               'PERFIL: '.
           03  WRK-CB3-PERFIL          PIC 9(008) VALUE ZEROS.
           03  FILLER                  PIC X(001) VALUE SPACES.
           03  WRK-CB3-NOME            PIC X(040) VALUE SPACES.

       01  WRK-CABEC4.
           03  FILLER                  PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(006) VALUE
               'DATA: '.
           03  WRK-CB4-DATA            PIC X(010) VALUE SPACES.
           03  FILLER                  PIC X(011) VALUE
               '  REMESSA: '.
           03  WRK-CB4-REMESSA         PIC 9(005) VALUE ZEROS.
           03  FILLER                  PIC X(011) VALUE
               '  RETORNO: '.
           03  WRK-CB4-RETORNO         PIC 9(005) VALUE ZEROS.
           03  FILLER                  PIC X(014) VALUE
               '  TP SERVICO: '.
           03  WRK-CB4-TP-SERV         PIC 9(002) VALUE ZEROS.
           03  FILLER                  PIC X(010) VALUE
               '  ORIGEM: '.
           03  WRK-CB4-ORIGEM          PIC 9(001) VALUE ZEROS.
           03  FILLER                  PIC X(014) VALUE
               '   TP PROCESS:'.
           03  WRK-CB4-TP-PROC         PIC 9(001) VALUE ZEROS.

       01  WRK-CABEC5.
           03  FILLER                  PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(019) VALUE
               'NUM CTRL ORIGINAL: '.
           03  WRK-CB5-NUM-CTRL        PIC X(020) VALUE ZEROS.

       01  WRK-CABEC6.
           03  FILLER                  PIC X(001) VALUE '0'.
           03  FILLER                  PIC X(130) VALUE
               'AGENCIA  CONTA  NUM.PAGAMENTO          CPF/CNPJ
      -        '    DATA                  VALOR  CIP COD MENSAGEM'.

       01  WRK-LINDET1    .
           03  WRK-LD1-CARRO            PIC X(001) VALUE SPACE.
           03  WRK-LD1-AGENCIA          PIC 9(005) VALUE ZEROS.
           03  FILLER                   PIC X(002) VALUE SPACES.
           03  WRK-LD1-CONTA            PIC 9(007) VALUE ZEROS.
           03  FILLER                   PIC X(002) VALUE SPACES.
           03  WRK-LD1-NUM-LIQUID       PIC X(021) VALUE SPACES.
           03  FILLER                   PIC X(002) VALUE SPACES.
           03  WRK-LD1-CNPJ             PIC X(016) VALUE SPACES.
           03  FILLER                   PIC X(002) VALUE SPACES.
           03  WRK-LD1-DATA             PIC X(010) VALUE SPACES.
           03  FILLER                   PIC X(002) VALUE SPACES.
           03  WRK-LD1-VALOR            PIC ZZZZ.ZZZ.ZZZ.ZZ9,99
                                                   VALUE ZEROS.
           03  FILLER                   PIC X(002) VALUE SPACES.
           03  WRK-LD1-COD-CIP          PIC X(002) VALUE SPACES.
           03  FILLER                   PIC X(002) VALUE SPACES.
           03  WRK-LD1-COD              PIC X(002) VALUE SPACES.
           03  FILLER                   PIC X(001) VALUE SPACES.
           03  WRK-LD1-MSG              PIC X(040) VALUE SPACES.

       01  WRK-LINDET1-99.
           03  FILLER                   PIC X(091) VALUE SPACES.
           03  WRK-LD1-COD-CIP-99       PIC X(002) VALUE SPACES.
           03  FILLER                   PIC X(002) VALUE SPACES.
           03  WRK-LD1-COD-99           PIC X(002) VALUE SPACES.
           03  FILLER                   PIC X(001) VALUE SPACES.
           03  WRK-LD1-MSG-99           PIC X(040) VALUE SPACES.

       01  WRK-LINTOT1    .
           03  WRK-LT1-CARRO            PIC X(001) VALUE '0'.
           03  FILLER                   PIC X(025) VALUE
               'TOTAL REGISTROS .......: '.
           03  WRK-LT1-TOTAL            PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           03  FILLER                   PIC X(025)      VALUE
               ' TOTAL VALOR   ........: '.
           03  WRK-LT1-VALOR            PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                       VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*** AREA DO RELTOTAL***'.
      *----------------------------------------------------------------*

       01  WRK-CABEC2-R3.
           03  FILLER                  PIC X(001) VALUE '0'.
           03  FILLER                  PIC X(029) VALUE
              'ASLC032 - RETORNO MULTIPAG - '.
           03  FILLER.
               05  WRK-CB2-R3-DIA      PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE '/'.
               05  WRK-CB2-R3-MES      PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE '/'.
               05  WRK-CB2-R3-ANO      PIC 9(004) VALUE ZEROS.
               05  FILLER              PIC X(003) VALUE ' - '.
               05  WRK-CB2-R3-HH       PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  WRK-CB2-R3-MM       PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  WRK-CB2-R3-SS       PIC 9(002) VALUE ZEROS.

       01  WRK-CABEC3-R3.
           03  FILLER                  PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(020) VALUE
               'PERFIL CREDEN...: '.
           03  WRK-CB3-R3-PERFIL       PIC 9(008) VALUE ZEROS.
           03  FILLER                  PIC X(001) VALUE SPACES.
           03  WRK-CB3-R3-NOME         PIC X(040) VALUE SPACES.

       01  WRK-LINTOT1-R3.
           03  FILLER                   PIC X(001) VALUE ' '.
           03  FILLER                   PIC X(020) VALUE
               'TOTAL INCONSIST.:   '.
           03  WRK-LT1-R3-QINC          PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           03  FILLER                   PIC X(022) VALUE
               '  VALOR INCONS...: '.
           03  WRK-LT1-R3-VINC          PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                   VALUE ZEROS.

       01  WRK-LINTOT2-R3.
           03  FILLER                   PIC X(001) VALUE ' '.
           03  FILLER                   PIC X(020) VALUE
               'TOTAL CONSIST...:   '.
           03  WRK-LT2-R3-QCON          PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           03  FILLER                   PIC X(022) VALUE
               '  VALOR CONSIST..: '.
           03  WRK-LT2-R3-VCON          PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                   VALUE ZEROS.

       01  WRK-LINTOT4-R3.
           03  FILLER                   PIC X(001) VALUE ' '.
           03  FILLER                   PIC X(020) VALUE
               'TOTAL PAGAMENTOS:   '.
           03  WRK-LT4-R3-QTOT          PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.
           03  FILLER                   PIC X(022) VALUE
               '  VALOR TOTAL....: '.
           03  WRK-LT4-R3-VTOT          PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                   VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)         VALUE
           '*     BOOK ARQUIVO DE SAIDA  *'.
      *----------------------------------------------------------------*

           COPY  'I#BVVEC6'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)         VALUE
           '*     MENSAGENS DE ERRO  *'.
      *----------------------------------------------------------------*

           COPY  'I#PFEB12'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)          VALUE
           '*  AREA DA POOL7100  *'.
      *----------------------------------------------------------------*

           COPY  'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM DA WORKING BVVE0278    *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA.

           PERFORM 2000-LER-REPAGFOR.

           IF  WRK-FS-REPAGFOR   EQUAL '10'
               DISPLAY   '***********BVVE0278******************* '
               DISPLAY   '*                                    * '
               DISPLAY   '*       ARQ    REPAGFOR   VAZIO      * '
               DISPLAY   '*         PROGRAMA ENCERRADO         * '
               DISPLAY   '*                                    * '
               DISPLAY   '************************************** '
           END-IF.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-REPAGFOR     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

           GOBACK.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT  REPAGFOR
               OUTPUT  RELINCO
                       RELCONS
                       RELTOTAL
                       ENVIACIP
                       REL99.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           PERFORM 1400-OBTER-DATA-CORRENTE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-TESTAR-FS-REPAGFOR.

           PERFORM 1220-TESTAR-FS-ENVIACIP.

           PERFORM 1230-TESTAR-FS-RELINCO.

           PERFORM 1240-TESTAR-FS-RELCONS.

           PERFORM 1250-TESTAR-FS-RELTOTAL.

           PERFORM 1260-TESTAR-FS-REL99.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-TESTAR-FS-REPAGFOR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REPAGFOR         NOT EQUAL '00'
               MOVE 'REPAGFOR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-REPAGFOR    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1220-TESTAR-FS-ENVIACIP          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENVIACIP          NOT EQUAL '00'
               MOVE 'ENVIACIP'          TO WRK-NOME-ARQ
               MOVE WRK-FS-ENVIACIP     TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO        TO ERR-TEXTO
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1230-TESTAR-FS-RELINCO          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELINCO          NOT EQUAL '00'
               MOVE 'RELINCO '         TO WRK-NOME-ARQ
               MOVE WRK-FS-RELINCO     TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1240-TESTAR-FS-RELCONS          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELCONS          NOT EQUAL '00'
               MOVE 'RELCONS '         TO WRK-NOME-ARQ
               MOVE WRK-FS-RELCONS     TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1250-TESTAR-FS-RELTOTAL          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELTOTAL          NOT EQUAL '00'
               MOVE 'RELTOTAL'          TO WRK-NOME-ARQ
               MOVE WRK-FS-RELTOTAL     TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1260-TESTAR-FS-REL99             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-REL99             NOT EQUAL '00'
               MOVE 'REL99'             TO WRK-NOME-ARQ
               MOVE WRK-FS-RELTOTAL     TO WRK-FILE-STATUS
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

           INITIALIZE                  WRK-DATA-HORA.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE  WRK-TIMESTAMP-AAAA TO  WRK-CB1-ANO    WRK-CB2-R3-ANO
           MOVE  WRK-TIMESTAMP-MM   TO  WRK-CB1-MES    WRK-CB2-R3-MES
           MOVE  WRK-TIMESTAMP-DD   TO  WRK-CB1-DIA    WRK-CB2-R3-DIA
           MOVE  WRK-TIMESTAMP-HH   TO  WRK-CB2-HORA   WRK-CB2-R2-HORA
                                        WRK-CB2-R3-HH
           MOVE  WRK-TIMESTAMP-MN   TO  WRK-CB2-MINUTO WRK-CB2-R2-MINUTO
                                        WRK-CB2-R3-MM
           MOVE  WRK-TIMESTAMP-SS   TO  WRK-CB2-SEGUNDO
                                        WRK-CB2-R2-SEGUNDO
                                        WRK-CB2-R3-SS.

           MOVE  WRK-DT-AAAAMMDD    TO WRK-DATA-9.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-LER-REPAGFOR               SECTION.
      *----------------------------------------------------------------*

           READ REPAGFOR               INTO BVVEC6-REGISTRO.

           IF  WRK-FS-REPAGFOR         EQUAL '10'
               GO TO 2000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-REPAGFOR.

           ADD     1                   TO ACU-LID-REPAGFOR.

           IF  BVVEC6-TP-REG           EQUAL '1'
               ADD 1                   TO ACU-LID-PGTO
SVK            IF  BVVEC6-INFO-RETORNO EQUAL SPACES
.                  DISPLAY '* REGISTRO COM OCORRENCIA LIXO *'
.                  MOVE '99'           TO BVVEC6-COD-ERRO(1)
.              END-IF
.              IF  BVVEC6-NUM-LIQUID   NOT NUMERIC
.                  DISPLAY '* REGISTRO COM NUMLIQUID LIXO *'
.                  DISPLAY '* ANTES.: ' BVVEC6-NUM-LIQUID
.                  PERFORM VARYING WRK-IND  FROM 1 BY 1
.                          UNTIL  (WRK-IND  GREATER 21)
.                      IF  BVVEC6-NUM-LIQUID(WRK-IND:1)
.                                      NOT NUMERIC
.                          MOVE '0'
.                                      TO BVVEC6-NUM-LIQUID(WRK-IND:1)
.                      END-IF
.                  END-PERFORM
.                  DISPLAY '* DEPOIS: ' BVVEC6-NUM-LIQUID
SVK            END-IF
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-FORMATAR-HEADER.

           PERFORM 2000-LER-REPAGFOR

           PERFORM 3200-PROCESSAR-TRANSACAO UNTIL
                  (BVVEC6-TP-REG      NOT EQUAL '1') OR
                   WRK-FS-REPAGFOR    EQUAL '10'.

           PERFORM 3400-EMITIR-TOTAIS.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-FORMATAR-HEADER            SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEC6-PERFIL             TO WRK-CB3-PERFIL
           MOVE BVVEC6-HE-NOME-EMPR(1:40) TO WRK-CB3-NOME
           MOVE BVVEC6-HE-NUM-REMESSA     TO WRK-CB4-REMESSA
           MOVE BVVEC6-HE-NUM-RETORNO     TO WRK-CB4-RETORNO
           MOVE BVVEC6-HE-DT-GRAV-ARQ     TO WRK-DATA-8N
           MOVE CORR WRK-DATA-8           TO WRK-DATA-DB2
           MOVE WRK-DATA-DB2              TO WRK-CB4-DATA
           MOVE BVVEC6-NUM-CTRL-ORIGINAL  TO WRK-CB5-NUM-CTRL
           MOVE BVVEC6-HE-TIP-SERVICO     TO WRK-CB4-TP-SERV
           MOVE BVVEC6-HE-ORIGEM-ARQ      TO WRK-CB4-ORIGEM
           MOVE BVVEC6-HE-TIP-PROCESS     TO WRK-CB4-TP-PROC.
           MOVE 99                        TO ACU-LINHAS
                                             ACU-LINHAS-2
                                             ACU-LINHAS-4.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-PROCESSAR-TRANSACAO        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-CONSIS WRK-AGENDADO
           MOVE BVVEC6-INFO-RETORNO    TO WRK-INFO-RETORNO
           MOVE SPACES                 TO BVVEC6-INFO-RETORNO

           PERFORM 3300-GERAR-ENVIACIP.

           IF  WRK-AGENDADO            EQUAL 'S'
               ADD  1                  TO ACU-AGENDADOS
           ELSE
               IF  WRK-CONSIS          EQUAL 'S'
                   PERFORM 3500-EMITIR-RELCONS
               ELSE
                   IF  WRK-TEM-99      EQUAL 'S'
                       MOVE BVVEC6-VALOR       TO WRK-9-13-COMP
                       ADD  WRK-9-13-COMP      TO ACU-VALOR-INCO-99
                       ADD 1                   TO ACU-GRA-REL99
                                                  ACU-TOT-RELINCO-99
                   ELSE
                       MOVE BVVEC6-VALOR       TO WRK-9-13-COMP
                       ADD  WRK-9-13-COMP      TO ACU-VALOR-INCO
                       ADD 1                   TO ACU-GRA-RELINCO
                                               ACU-TOT-RELINCO
                   END-IF
                   PERFORM 3210-FORMATA-LINDET
                   PERFORM VARYING WRK-IND  FROM 1 BY 1
                           UNTIL  (WRK-IND  GREATER 5)
                           OR     (WRK-INFO-RET (WRK-IND) EQUAL SPACES)
                       IF  WRK-TEM-99      EQUAL 'S'
                           PERFORM 3700-EMITIR-REL99
                       ELSE
                           PERFORM 3600-EMITIR-RELINCO
                       END-IF
                   END-PERFORM
               END-IF
           END-IF.

           PERFORM 2000-LER-REPAGFOR.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3210-FORMATA-LINDET             SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEC6-DATA            TO WRK-DATA-8N
           MOVE CORR WRK-DATA-8        TO WRK-DATA-DB2
           MOVE WRK-DATA-DB2           TO WRK-LD1-DATA

           MOVE BVVEC6-VALOR           TO WRK-LD1-VALOR
           MOVE BVVEC6-AGENCIA         TO WRK-LD1-AGENCIA.
           MOVE BVVEC6-CONTA           TO WRK-LD1-CONTA.
           MOVE BVVEC6-NUM-LIQUID      TO WRK-LD1-NUM-LIQUID

           IF  BVVEC6-FILIAL           NOT EQUAL ZEROS
               MOVE BVVEC6-CNPJ-CPF(2:8) 
                                       TO WRK-CCNPJ
               MOVE BVVEC6-FILIAL      TO WRK-CFLIAL
               MOVE BVVEC6-CTRL        TO WRK-CCTRL-CNPJ
               MOVE '/'                TO WRK-BARRA
               MOVE '-'                TO WRK-TRACO-1
               MOVE  WRK-CPF-CPNJ-EDIT TO WRK-LD1-CNPJ
           ELSE
               MOVE BVVEC6-CNPJ-CPF    TO WRK-CCPF
               MOVE BVVEC6-CTRL        TO WRK-CCTRL-CPF
               MOVE '-'                TO WRK-TRACO-2
               MOVE  WRK-CPF-EDIT      TO WRK-LD1-CNPJ
           END-IF.

           MOVE WRK-LINDET1            TO WRK-LINDET1-99.


      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-GERAR-ENVIACIP            SECTION.
      *----------------------------------------------------------------*

      *    VERIFICA SE AGENDADMENTO OK
      *    SE SIM, DESPREZA POIS PARA PAGAMENTOS DO DIA
           IF  BVVEC6-NIVEL-INFO   EQUAL '3' AND (
               WRK-INFO-RET(1)  EQUAL 'BD' OR
               WRK-INFO-RET(1)  EQUAL 'BT' OR
               WRK-INFO-RET(1)  EQUAL 'BU' OR
               WRK-INFO-RET(1)  EQUAL 'BV' OR
               WRK-INFO-RET(1)  EQUAL 'FS' OR
               WRK-INFO-RET(1)  EQUAL 'GU' OR
               WRK-INFO-RET(1)  EQUAL 'HA' OR
               WRK-INFO-RET(1)  EQUAL 'JL' OR
               WRK-INFO-RET(1)  EQUAL 'KT' OR
               WRK-INFO-RET(1)  EQUAL 'LA' OR
               WRK-INFO-RET(1)  EQUAL 'ME' OR
               WRK-INFO-RET(1)  EQUAL 'TR' )
               MOVE 'S'            TO WRK-AGENDADO
               GO TO 3300-99-FIM
           ELSE
      *        VERIFICA SE PAGAMENTO OK
               IF  BVVEC6-NIVEL-INFO   EQUAL '3' AND (
                   WRK-INFO-RET(1)  EQUAL 'BW' OR
                   WRK-INFO-RET(1)  EQUAL 'BT' OR
                   WRK-INFO-RET(1)  EQUAL 'BU' OR
                   WRK-INFO-RET(1)  EQUAL 'BV' OR
                   WRK-INFO-RET(1)  EQUAL 'HF' OR
                   WRK-INFO-RET(1)  EQUAL 'JL' OR
                   WRK-INFO-RET(1)  EQUAL 'TR' )
                   MOVE SPACES         TO BVVEC6-INFO-RETORNO
                   MOVE '00'           TO BVVEC6-COD-ERRO(1)
                   MOVE 'S'            TO WRK-CONSIS
               ELSE
                   MOVE SPACES         TO BVVEC6-INFO-RETORNO
                   MOVE SPACES         TO WRK-TEM-99
                   PERFORM 3310-VER-ERRO VARYING WRK-IND
                           FROM 1      BY 1 UNTIL
                          (WRK-IND     GREATER 5) OR
                          (WRK-INFO-RET(WRK-IND) EQUAL SPACES OR
                                                       LOW-VALUES)
               END-IF
           END-IF.

           MOVE WRK-GRAVACAO               TO WRK-OPERACAO.
           MOVE WRK-CB3-NOME(1:30)         TO BVVEC6-NOME-CRED
           WRITE FD-REG-ENVIACIP           FROM BVVEC6-REGISTRO.
           PERFORM 1220-TESTAR-FS-ENVIACIP.
           ADD 1                           TO ACU-GRA-ENVIACIP.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3310-VER-ERRO                   SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-INFO-RET(WRK-IND)
               WHEN 'BI'
               WHEN 'BJ'
               WHEN 'BM'
               WHEN 'BN'
               WHEN 'BQ'
               WHEN 'HE'
                  MOVE '03'                  TO BVVEC6-COD-ERRO(WRK-IND)

               WHEN 'AL'
                  MOVE '10'                  TO BVVEC6-COD-ERRO(WRK-IND)

               WHEN 'AM'
                  MOVE '08'                  TO BVVEC6-COD-ERRO(WRK-IND)

               WHEN 'AN'
                  MOVE '04'                  TO BVVEC6-COD-ERRO(WRK-IND)

               WHEN 'AT'
               WHEN 'GP'
                  MOVE '11'                  TO BVVEC6-COD-ERRO(WRK-IND)

               WHEN 'BG'
                  MOVE '12'                  TO BVVEC6-COD-ERRO(WRK-IND)

               WHEN 'FL'
                  MOVE '32'                  TO BVVEC6-COD-ERRO(WRK-IND)

               WHEN 'GD'
               WHEN 'GE'
               WHEN 'GJ'
               WHEN 'GK'
               WHEN 'GL'
               WHEN 'GW'
               WHEN 'GY'
               WHEN 'MB'
               WHEN 'MC'
                  MOVE '05'                  TO BVVEC6-COD-ERRO(WRK-IND)

               WHEN OTHER
                  MOVE '99'                  TO BVVEC6-COD-ERRO(WRK-IND)
                  MOVE 'S'                   TO WRK-TEM-99

           END-EVALUATE.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3400-EMITIR-TOTAIS               SECTION.
      *----------------------------------------------------------------*

           IF  ACU-GRA-RELINCO          GREATER ZEROS
               MOVE   ACU-GRA-RELINCO   TO WRK-LT1-TOTAL
               MOVE   ACU-VALOR-INCO    TO WRK-LT1-VALOR
               WRITE  FD-REG-RELINCO    FROM WRK-LINTOT1
               PERFORM 1230-TESTAR-FS-RELINCO
           END-IF.

           IF  ACU-GRA-RELCONS          GREATER ZEROS
               MOVE   ACU-GRA-RELCONS   TO WRK-LT1-TOTAL
               MOVE   ACU-VALOR-CON     TO WRK-LT1-VALOR
               WRITE  FD-REG-RELCONS    FROM WRK-LINTOT1
               PERFORM 1240-TESTAR-FS-RELCONS
           END-IF.

           IF  ACU-GRA-REL99            GREATER ZEROS
               MOVE   ACU-GRA-REL99     TO WRK-LT1-TOTAL
               MOVE   ACU-VALOR-INCO-99 TO WRK-LT1-VALOR
               WRITE  FD-REG-REL99      FROM WRK-LINTOT1
               PERFORM 1260-TESTAR-FS-REL99
           END-IF.


           IF  ACU-LINHAS-3            GREATER 60
               WRITE FD-REG-RELTOTAL   FROM WRK-CABEC2-R3
               PERFORM 1250-TESTAR-FS-RELTOTAL
               MOVE 2                  TO ACU-LINHAS-3
           END-IF.

           MOVE SPACES                 TO  FD-REG-RELTOTAL
           WRITE FD-REG-RELTOTAL
           PERFORM 1250-TESTAR-FS-RELTOTAL

           MOVE WRK-CB3-PERFIL         TO WRK-CB3-R3-PERFIL
           MOVE WRK-CB3-NOME           TO WRK-CB3-R3-NOME
           WRITE FD-REG-RELTOTAL       FROM WRK-CABEC3-R3
           PERFORM 1250-TESTAR-FS-RELTOTAL

           MOVE ACU-GRA-RELINCO        TO WRK-LT1-R3-QINC
           MOVE ACU-VALOR-INCO         TO WRK-LT1-R3-VINC
           WRITE FD-REG-RELTOTAL       FROM WRK-LINTOT1-R3
           PERFORM 1250-TESTAR-FS-RELTOTAL

           MOVE ACU-GRA-RELCONS        TO WRK-LT2-R3-QCON
           MOVE ACU-VALOR-CON          TO WRK-LT2-R3-VCON
           WRITE FD-REG-RELTOTAL       FROM WRK-LINTOT2-R3
           PERFORM 1250-TESTAR-FS-RELTOTAL

           ADD  ACU-GRA-RELINCO        TO ACU-GRA-RELCONS
           MOVE ACU-GRA-RELCONS        TO WRK-LT4-R3-QTOT
           ADD  ACU-VALOR-INCO         TO ACU-VALOR-CON
           MOVE ACU-VALOR-CON          TO WRK-LT4-R3-VTOT
           WRITE FD-REG-RELTOTAL       FROM WRK-LINTOT4-R3
           PERFORM 1250-TESTAR-FS-RELTOTAL.

           MOVE ZEROS                  TO  ACU-GRA-RELINCO
                                           ACU-GRA-RELCONS
                                           ACU-VALOR-INCO
                                           ACU-VALOR-CON
                                           ACU-GRA-REL99
                                           ACU-VALOR-INCO-99.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-EMITIR-RELCONS             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  ACU-LINHAS-2            GREATER 57
               PERFORM 3511-IMPRIMIR-CABECALHO
           END-IF.

           PERFORM 3210-FORMATA-LINDET

           MOVE SPACES                 TO WRK-LD1-MSG
           MOVE WRK-INFO-RET(1)        TO WRK-LD1-COD

           PERFORM VARYING WRK-IND-2   FROM 1 BY 1
                   UNTIL   WRK-IND-2   GREATER WRK-QTDE-MSG
              IF WRK-LD1-COD           EQUAL TAB-COD-MENS(WRK-IND-2)
                 MOVE TAB-MENSAGEM(WRK-IND-2)(1:40) TO WRK-LD1-MSG
                 MOVE 900              TO WRK-IND-2
              END-IF
           END-PERFORM

           WRITE FD-REG-RELCONS        FROM WRK-LINDET1
           PERFORM 1240-TESTAR-FS-RELCONS.

           MOVE BVVEC6-VALOR           TO WRK-9-13-COMP.
           ADD  WRK-9-13-COMP          TO ACU-VALOR-CON

           ADD 1                       TO ACU-LINHAS-2
                                          ACU-GRA-RELCONS
                                          ACU-TOT-RELCONS.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3511-IMPRIMIR-CABECALHO         SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS-2

           MOVE ACU-PAGINAS-2          TO WRK-CB1-PAGINA

           WRITE FD-REG-RELCONS        FROM WRK-CABEC1
           PERFORM 1240-TESTAR-FS-RELCONS

           WRITE FD-REG-RELCONS        FROM WRK-CABEC2
           PERFORM 1240-TESTAR-FS-RELCONS

           WRITE FD-REG-RELCONS        FROM WRK-CABEC3
           PERFORM 1240-TESTAR-FS-RELCONS

           WRITE FD-REG-RELCONS        FROM WRK-CABEC4
           PERFORM 1240-TESTAR-FS-RELCONS

           WRITE FD-REG-RELCONS        FROM WRK-CABEC5
           PERFORM 1240-TESTAR-FS-RELCONS

           WRITE FD-REG-RELCONS        FROM WRK-CABEC6
           PERFORM 1240-TESTAR-FS-RELCONS

           MOVE 8                      TO ACU-LINHAS-2.

      *----------------------------------------------------------------*
       3511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3600-EMITIR-RELINCO             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  ACU-LINHAS              GREATER 57
               PERFORM 3610-IMPRIMIR-CABECALHO
           END-IF.

           MOVE WRK-INFO-RET(WRK-IND)     TO WRK-LD1-COD
           MOVE BVVEC6-COD-ERRO(WRK-IND)  TO WRK-LD1-COD-CIP

           PERFORM VARYING WRK-IND-2   FROM 1 BY 1
                   UNTIL   WRK-IND-2   GREATER WRK-QTDE-MSG
              IF WRK-INFO-RET(WRK-IND) EQUAL TAB-COD-MENS(WRK-IND-2)
                 MOVE TAB-MENSAGEM(WRK-IND-2)(1:40) TO WRK-LD1-MSG
                 MOVE 900              TO WRK-IND-2
              END-IF
           END-PERFORM

           WRITE FD-REG-RELINCO        FROM WRK-LINDET1
           PERFORM 1230-TESTAR-FS-RELINCO.

           ADD 1                       TO ACU-LINHAS

           MOVE SPACES                 TO WRK-LINDET1.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3610-IMPRIMIR-CABECALHO         SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS

           MOVE ACU-PAGINAS            TO WRK-CB1-PAGINA

           WRITE FD-REG-RELINCO        FROM WRK-CABEC1
           PERFORM 1230-TESTAR-FS-RELINCO

           WRITE FD-REG-RELINCO        FROM WRK-CABEC2-R2
           PERFORM 1230-TESTAR-FS-RELINCO

           WRITE FD-REG-RELINCO        FROM WRK-CABEC3
           PERFORM 1230-TESTAR-FS-RELINCO

           WRITE FD-REG-RELINCO        FROM WRK-CABEC4
           PERFORM 1230-TESTAR-FS-RELINCO

           WRITE FD-REG-RELINCO        FROM WRK-CABEC5
           PERFORM 1230-TESTAR-FS-RELINCO

           WRITE FD-REG-RELINCO        FROM WRK-CABEC6
           PERFORM 1230-TESTAR-FS-RELINCO

           MOVE 8                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3700-EMITIR-REL99               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  ACU-LINHAS-4            GREATER 57
               PERFORM 3710-IMPRIMIR-CABECALHO
           END-IF.

           MOVE WRK-INFO-RET(WRK-IND)     TO WRK-LD1-COD-99
           MOVE BVVEC6-COD-ERRO(WRK-IND)  TO WRK-LD1-COD-CIP-99

           PERFORM VARYING WRK-IND-2   FROM 1 BY 1
                   UNTIL   WRK-IND-2   GREATER WRK-QTDE-MSG
              IF WRK-INFO-RET(WRK-IND) EQUAL TAB-COD-MENS(WRK-IND-2)
                 MOVE TAB-MENSAGEM(WRK-IND-2)(1:40) TO WRK-LD1-MSG-99
                 MOVE 900              TO WRK-IND-2
              END-IF
           END-PERFORM

           WRITE FD-REG-REL99          FROM WRK-LINDET1-99
           PERFORM 1260-TESTAR-FS-REL99.

           ADD 1                       TO ACU-LINHAS-4.
           MOVE SPACES                 TO WRK-LINDET1-99.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3710-IMPRIMIR-CABECALHO         SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS-4

           MOVE ACU-PAGINAS-4          TO WRK-CB1-PAGINA

           WRITE FD-REG-REL99          FROM WRK-CABEC1
           PERFORM 1260-TESTAR-FS-REL99

           WRITE FD-REG-REL99          FROM WRK-CABEC2-R2
           PERFORM 1260-TESTAR-FS-REL99

           WRITE FD-REG-REL99          FROM WRK-CABEC3
           PERFORM 1260-TESTAR-FS-REL99

           WRITE FD-REG-REL99          FROM WRK-CABEC4
           PERFORM 1260-TESTAR-FS-REL99

           WRITE FD-REG-REL99          FROM WRK-CABEC5
           PERFORM 1260-TESTAR-FS-REL99

           WRITE FD-REG-REL99          FROM WRK-CABEC6
           PERFORM 1260-TESTAR-FS-REL99

           MOVE 8                      TO ACU-LINHAS-4.

      *----------------------------------------------------------------*
       3710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE REPAGFOR
                 RELINCO
                 RELCONS
                 RELTOTAL
                 ENVIACIP
                 REL99.

           MOVE WRK-FECHAMENTO         TO  WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           GOBACK.
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           IF  ACU-GRA-RELTOTAL          GREATER ZEROS
               MOVE   ACU-GRA-RELTOTAL   TO WRK-LT1-TOTAL
               MOVE   ACU-VALOR-AGEN    TO WRK-LT1-VALOR
               WRITE  FD-REG-RELTOTAL    FROM WRK-LINTOT1
               PERFORM 1250-TESTAR-FS-RELTOTAL
           END-IF.

           DISPLAY '*************** BVVE0278 ******************'
           DISPLAY '*                                         *'

           MOVE ACU-LID-REPAGFOR      TO WRK-MASCARA.
           DISPLAY '*   LIDOS (HEAD/TRAIL) : ' WRK-MASCARA '      *'

           MOVE ACU-LID-PGTO          TO WRK-MASCARA.
           DISPLAY '*   LIDOS (PAGAMENTOS) : ' WRK-MASCARA '      *'

           MOVE ACU-AGENDADOS          TO WRK-MASCARA.
           DISPLAY '*   DESPRE. AGENDAMENTO: ' WRK-MASCARA '      *'

           MOVE ACU-TOT-RELINCO       TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA RELINCO : ' WRK-MASCARA '      *'

           MOVE ACU-TOT-RELCONS       TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA RELCONS : ' WRK-MASCARA '      *'

           MOVE ACU-GRA-ENVIACIP      TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA ENVIACIP: ' WRK-MASCARA '      *'

           MOVE ACU-GRA-RELTOTAL       TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA RELTOTAL: ' WRK-MASCARA '      *'
           MOVE ACU-TOT-RELINCO-99          TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA REL99   : ' WRK-MASCARA '      *'
           DISPLAY '*                                         *'
           DISPLAY '*************** BVVE0278 ******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE0278'             TO ERR-PGM.

           CALL 'BRAD7100'         USING WRK-BATCH
                                         ERRO-AREA.

           GOBACK.
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
