      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE0609.
       AUTHOR.     FRANCISMARA PAGNONCELLI.
      *                                                                *
      *================================================================*
      *                  B S I  T E C N O L O G I A                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....: BVVE0609                                      *
      *    PROGRAMADORA: FRANCISMARA PAGNONCELLI  - BSI                *
      *    ANALISTA....: FRANCISMARA PAGNONCELLI  - BSI                *
      *    DATA........: JANEIRO/2020                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....: VERIFICAR OS CNPJS QUE NAO TEM CONTRATO ATIVO *
      *    PARA ENVIAR A DESTRAVA                                      *
      *----------------------------------------------------------------*
      *    ARQUIVOS     :                                              *
      *      DDNAME                                INCLUDE/BOOK        *
      *       CONTRATI                                I#BVVEMM         *
      *       TRAVPATI                                I#BVVEDW         *
      *       DESTRAVA                                I#BVVEMA         *
      *       RELDESTR                                   -             *
      *================================================================*
111120*                      A L T E R A C O E S                       *
111120*----------------------------------------------------------------*
111120*   DATA     EMPRESA   ALTERACAO REALIZADA                       *
111120*----------------------------------------------------------------*
111120* 22/10/20   FOURSYS   IN4823464 - DECLARAR BOOK INTERNO (COPIA  *
111120*                      DO I#BVVEMA) AUMENTANDO O CAMPO CPF/CNPJ  *
111120*                      PARA TRATAMENTO CONFORME ABAIXO:          *
111120*                           CPF  -> NNNNNNNNN0000DD              *
111120*                           CNPJ -> NNNNNNNNNFFFFDD              *
111120*================================================================*

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

           SELECT CONTRATI ASSIGN      TO UT-S-CONTRATI
                    FILE STATUS        IS WRK-FS-CONTRATI.

           SELECT TRAVPATI ASSIGN      TO TRAVPATI
                    FILE STATUS        IS WRK-FS-TRAVPATI.

           SELECT DESTRAVA ASSIGN      TO UT-S-DESTRAVA
                    FILE STATUS        IS WRK-FS-DESTRAVA.

           SELECT RELDESTR ASSIGN      TO UT-S-RELDESTR
                    FILE STATUS        IS WRK-FS-RELDESTR.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT - ARQUIVO DE CONTRATOS ATIVOS                         *
      *            ORG. SEQUENCIAL - LRECL = 100                       *
      *----------------------------------------------------------------*

       FD  CONTRATI
           RECORDING  MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-CONTRATI.
           05  FILLER               PIC  X(100).

      *----------------------------------------------------------------*
      *    INPUT - ARQUIVO DE TRAVAS ATIVAS                            *
      *            ORG. SEQUENCIAL - LRECL = 300                       *
      *----------------------------------------------------------------*

       FD  TRAVPATI
           RECORDING  MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-TRAVPATI.
           05  FILLER               PIC  X(300).

      *----------------------------------------------------------------*
      *    OUTPUT- ARQUIVO SAIDA DESTRAVA                              *
      *            ORG. FIXO - LRECL = 150                             *
      *----------------------------------------------------------------*

       FD  DESTRAVA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-DESTRAVA            PIC  X(150).

      *----------------------------------------------------------------*
      *    OUTPUT- RELATORIO DE DESTRAVAS ENVIADAS                     *
      *            ORG. FIXO - LRECL = 133                             *
      *----------------------------------------------------------------*

       FD  RELDESTR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELDESTR            PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVE0609 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-MULADORES.
           03  ACU-LID-CONTRATI        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LID-TRAVPATI        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-DESTRAVA        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-RELDESTR        PIC  9(09) COMP-3   VALUE ZEROS.

       77  ACU-LINHAS                  PIC  9(007) COMP-3  VALUE 99.
       77  ACU-PAGINAS                 PIC  9(004) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE OPERACAO DE ARQUIVOS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-CONTRATI             PIC X(02)           VALUE SPACES.
       77  WRK-FS-TRAVPATI             PIC X(02)           VALUE SPACES.
       77  WRK-FS-DESTRAVA             PIC X(02)           VALUE SPACES.
       77  WRK-FS-RELDESTR             PIC X(02)           VALUE SPACES.

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
       77  WRK-CHV-CONTR-ANT           PIC  X(12)        VALUE SPACES.
       77  WRK-CHV-TRAV-ANT            PIC  X(12)        VALUE SPACES.

7C2511 01  WRK-CHV-CONTR.
7C2511     05  WRK-CCNPJ-CONTR         PIC  X(009)     VALUE SPACES.
7C2511     05  WRK-BAND-CONTR          PIC  9(003)     VALUE ZEROS.
      *
      *01  WRK-CHV-TRAV.
      *    05  WRK-CCNPJ-TRAV          PIC  9(009)     VALUE ZEROS.
      *    05  WRK-BAND-TRAV           PIC  X(003)     VALUE SPACES.
7C2511 01  WRK-CHV-TRAV.
7C2511     05  WRK-CCNPJ-TRAV          PIC  X(009)     VALUE SPACES.
7C2511     05  WRK-BAND-TRAV           PIC  X(003)     VALUE SPACES.

       01  WRK-DATA-MOVTO              PIC 9(08)         VALUE ZEROS.

       01  WRK-DATA-TIMES.
           03  WRK-ANO                 PIC 9(04)         VALUE ZEROS.
           03  FILLER                  PIC X(01)         VALUE '-'.
           03  WRK-MES                 PIC 9(02)         VALUE ZEROS.
           03  FILLER                  PIC X(01)         VALUE '-'.
           03  WRK-DIA                 PIC 9(02)         VALUE ZEROS.
       01  WRK-DATA-TIMES-R REDEFINES  WRK-DATA-TIMES
                                       PIC X(10).
       01  WRK-DATA-DB2.
           03  WRK-DIA                 PIC 9(02)         VALUE ZEROS.
           03  FILLER                  PIC X(01)         VALUE '/'.
           03  WRK-MES                 PIC 9(02)         VALUE ZEROS.
           03  FILLER                  PIC X(01)         VALUE '/'.
           03  WRK-ANO                 PIC 9(04)         VALUE ZEROS.

       01  WRK-DATA-INV                PIC 9(08)         VALUE ZEROS.
       01  WRK-DATA-INV-R REDEFINES  WRK-DATA-INV.
           03  WRK-ANO                 PIC 9(04).
           03  WRK-MES                 PIC 9(02).
           03  WRK-DIA                 PIC 9(02).

111120*01  WRK-CPFCNPJ-NUM             PIC 9(15)         VALUE ZEROS.
111120*01  FILLER         REDEFINES  WRK-CPFCNPJ-NUM.
111120*    03  WRK-CPFCNPJ             PIC 9(09).
111120*    03  WRK-FILIAL              PIC 9(04).
111120*    03  WRK-DIGITO              PIC 9(02).
7C2511 01  WRK-CPFCNPJ-NUM             PIC X(15)         VALUE SPACES.
7C2511 01  FILLER         REDEFINES  WRK-CPFCNPJ-NUM.
7C2511     03  WRK-CPFCNPJ             PIC X(09).
7C2511     03  WRK-FILIAL              PIC X(04).
7C2511     03  WRK-DIGITO              PIC 9(02).

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
             07 WRK-TIMESTAMP-DATA     PIC  9(08)   VALUE ZEROS.
             07 FILLER REDEFINES WRK-TIMESTAMP-DATA.
                10 WRK-TIMESTAMP-AAAA  PIC  9(04).
                10 WRK-TIMESTAMP-MM    PIC  9(02).
                10 WRK-TIMESTAMP-DD    PIC  9(02).
             07 WRK-TIMESTAMP-HH       PIC  9(02)   VALUE ZEROS.
             07 WRK-TIMESTAMP-MN       PIC  9(02)   VALUE ZEROS.
             07 WRK-TIMESTAMP-SS       PIC  9(02)   VALUE ZEROS.
             07 WRK-TIMESTAMP-MIL      PIC  9(06)   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '* AREA I#BVVEMM  *'.
      *----------------------------------------------------------------*

       COPY    'I#BVVEMM'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '* AREA I#BVVEMA  *'.
      *----------------------------------------------------------------*

111120*----------------------------------------------------------------*
111120*  I#BVVEMA - MANUTENCAO DE DOMICILIO BANCARIO - EM MASSA        *
111120*  FORMATO DATAS: AAAAMMDD                                       *
111120*----------------------------------------------------------------*
111120
111120*  O BOOK I#BVVEMA FOI DECLARADO INTERNAMENTE NO PROGRAMA PARA
111120*  AUMENTAR O CAMPO DO CPFCNPJ. PASSOU DE 14 PARA 15 POSICOES
111120*  RETIRADO 1 CAMPO DO FILLER. ESSA ALTERACAO SE FEZ NECESSARIA
111120*  PARA TRATAR O IN4823464. O CPF DEVERA SER NNNNNNNNN0000DD E
111120*  O CNPJ NNNNNNNNNFFFFDD PARA FUTURA CLASSIFICACAO
111120
111120 01  WRK-BVVEMA-REGISTRO.
111120     03  WRK-BVVEMA-ROTINA               PIC X(004) VALUE SPACES.
111120     03  WRK-BVVEMA-ORIGEM               PIC X(001) VALUE SPACES.
111120     03  WRK-BVVEMA-DATA                 PIC 9(008) VALUE ZEROS.
111120     03  WRK-BVVEMA-SEQ                  PIC 9(003) VALUE ZEROS.
111120     03  WRK-BVVEMA-COD-MSG              PIC X(007) VALUE SPACES.
111120     03  WRK-BVVEMA-NUMCTRL-IF-ENVIO     PIC X(020) VALUE SPACES.
111120     03  WRK-BVVEMA-SCG0013.
111120         05  WRK-BVVEMA-E-DADOS-CLI.
111120             07 WRK-BVVEMA-E-TP-CLI      PIC X(001) VALUE SPACES.
111120*            07 WRK-BVVEMA-E-CNPJCPF-CLI PIC 9(015) VALUE ZEROS.
7C2511             07 WRK-BVVEMA-E-CNPJCPF-CLI PIC X(015) VALUE SPACES.
111120         05  WRK-BVVEMA-E-TRAVA-RAIZ     PIC X(001) VALUE SPACES.
111120         05  WRK-BVVEMA-E-NUM-IDENTC-SCG PIC 9(017) VALUE ZEROS.
111120         05  WRK-BVVEMA-E-TP-BAIXA-DOMCL PIC X(001) VALUE SPACES.
111120         05  WRK-BVVEMA-E-DT-MOVTO       PIC 9(008) VALUE ZEROS.
111120         05  WRK-BVVEMA-E-BANDEIRA       PIC 9(003) VALUE ZEROS.
111120         05  WRK-BVVEMA-E-COD-PROPT-ESQM PIC X(003) VALUE SPACES.
111120         05  FILLER                      PIC X(058) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '* AREA I#BVVEDW  *'.
      *----------------------------------------------------------------*

      *XOPY    'I#BVVEDW'.
      *================================================================*
      *  I#BVVEDW - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  TRAVAS DE DOMICILIO ATIVAS                                    *
      *----------------------------------------------------------------*
      *  DSNAME       : CB.BACC.VON.TRAVPATI                           *
      *  LRECL ARQUIVO: 300                                            *
      *  LRECL CHAVE  : 060                                            *
      *----------------------------------------------------------------*
      *  FORMATOS                                                      *
      *   DATA        : AAAAMMDD                                       *
      *   TIMESTAMP   : AAAA-MM-DDTHH:MM:SS                            *
      *================================================================*

       01  WRK-DW-TRAVPATI.
           05 WRK-DW-CHAVE.
      *       10 WRK-DW-CH-CNPJ-CPF            PIC  9(009) VALUE 0.
      *       10 WRK-DW-CH-FILIAL              PIC  9(004) VALUE 0.
7C2511        10 WRK-DW-CH-CNPJ-CPF            PIC  X(009) VALUE SPACES.
7C2511        10 WRK-DW-CH-FILIAL              PIC  X(004) VALUE SPACES.
7C2511        10 FILLER                        PIC  X(001) VALUE SPACES.
              10 WRK-DW-CH-COD-BAND            PIC  9(003) VALUE 0.
              10 WRK-DW-CH-FILLER              PIC  X(044) VALUE SPACES.
           05 WRK-DW-NUM-IDENT-SCG             PIC  9(017) VALUE 0.
           05 WRK-DW-TP-PSSOA                  PIC  X(001) VALUE SPACES.
           05 WRK-DW-TP-PRODT                  PIC  X(001) VALUE SPACES.
           05 WRK-DW-AG-CLI                    PIC  9(004) VALUE 0.
           05 WRK-DW-CTA-CLI                   PIC  9(013) VALUE 0.
           05 WRK-DW-PERC-GAR                  PIC  9(003)V99  VALUE 0.
           05 WRK-DW-DTINI-VALIDD              PIC  9(008) VALUE 0.
           05 WRK-DW-DTFIM-VALIDD              PIC  9(008) VALUE 0.
           05 WRK-DW-TIMES-MANUT               PIC  X(020) VALUE SPACES.
           05 WRK-DW-CTRL-CNPJ-CPF             PIC  9(002) VALUE 0.
           05 WRK-DW-COD-PROP-ESQM             PIC  X(003) VALUE SPACES.
           05 WRK-DW-VLR-MAX-RET               PIC  9(013)V99 VALUE 0.
           05 FILLER                           PIC  X(143) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*** AREA DO RELACONS ***'.
      *----------------------------------------------------------------*

       01  WRK-CABEC1-R2.
           03  CB1-CARRO-R2            PIC X(001) VALUE '1'.
           03  CB1-DATA-R2.
               05  CB1-DIA-R2          PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE '/'.
               05  CB1-MES-R2          PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE '/'.
               05  CB1-ANO-R2          PIC 9(004) VALUE ZEROS.
           03  FILLER                  PIC X(040) VALUE SPACES.
           03  FILLER                  PIC X(060) VALUE
              'BRADESCO CARTOES'.
           03  FILLER                  PIC X(008) VALUE
              'PAGINA: '.
           03  CB1-PAGINA-R2           PIC ZZZ9   VALUE ZEROS.

       01  WRK-CABEC2-R2.
           03  CB2-CARRO-R2            PIC X(001) VALUE ' '.
           03  FILLER                  PIC X(008) VALUE 'BVVE0609'.
           03  FILLER                  PIC X(027) VALUE SPACES.
           03  FILLER                  PIC X(063) VALUE
           'SOLICITACOES DE EXCLUSAO DE TRAVA EM MASSA ENVIADAS A CIP'.
           03  FILLER                  PIC X(016) VALUE SPACES.
           03  CB2-HORAS-R2.
               05  CB2-HORA-R2         PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  CB2-MINUTO-R2       PIC 9(002) VALUE ZEROS.
               05  FILLER              PIC X(001) VALUE ':'.
               05  CB2-SEGUNDO-R2      PIC 9(002) VALUE ZEROS.

       01  WRK-CABEC3-R2.
           03  CB3-CARRO-R2             PIC X(001) VALUE '0'.
           03  FILLER                   PIC X(002) VALUE SPACES.
           03  FILLER                   PIC X(006) VALUE
               'DATA: '.
           03  CB3-DATA-R2              PIC X(010) VALUE SPACES.

       01  WRK-CABEC4-R2.
           03  CB4-CARRO-R2             PIC X(001) VALUE '0'.
           03  FILLER                   PIC X(005) VALUE SPACES.
           03  FILLER                   PIC X(120) VALUE
               '    CNPJ/CPF       BAND     AGENCIA   CONTA     INI TRAV
      -        'A   FIM TRAVA      NUM SCG'.

       01  WRK-LINDET1-R2 .
           03  LD1-CARRO-R2             PIC X(001) VALUE SPACE.
           03  FILLER                   PIC X(003) VALUE SPACES.
           03  LD1-TP-PSSOA-R2          PIC X(001) VALUE SPACES.
           03  FILLER                   PIC X(001) VALUE SPACES.
           03  LD1-CNPJ-CPF-R2          PIC 9(013) VALUE ZEROS.
           03  FILLER                   PIC X(001) VALUE '-'.
           03  LD1-DIG-R2               PIC 9(002) VALUE ZEROS.
           03  FILLER                   PIC X(003) VALUE SPACES.
           03  LD1-BAND-R2              PIC X(006) VALUE SPACES.
           03  FILLER                   PIC X(005) VALUE SPACES.
           03  LD1-AGENCIA-R2           PIC ZZ999  VALUE ZEROS.
           03  FILLER                   PIC X(002) VALUE SPACES.
           03  LD1-CONTA-R2             PIC ZZZZ999  VALUE ZEROS.
           03  FILLER                   PIC X(01)    VALUE '-'.
           03  LD1-DIG-CTA              PIC 9(01)    VALUE ZEROS.
           03  FILLER                   PIC X(003) VALUE SPACES.
           03  LD1-DT-INI-R2            PIC X(010) VALUE SPACES.
           03  FILLER                   PIC X(002) VALUE SPACES.
           03  LD1-DT-FIM-R2            PIC X(010) VALUE SPACES.
           03  FILLER                   PIC X(003) VALUE SPACES.
           03  LD1-NUM-SCG-R2           PIC 9(017) VALUE ZEROS.

       01  WRK-LINTOT1-R2    .
           03  WRK-LT1-CARRO-R2          PIC X(001) VALUE '0'.
           03  FILLER                   PIC X(030) VALUE
               'TOTAL REGISTROS ............: '.
           03  WRK-LT1-TOTAL-R2         PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)          VALUE
           '*  AREA DA POOL7100  *'.
      *----------------------------------------------------------------*

           COPY  'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM DA WORKING BVVE0609    *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA.

           PERFORM 2000-TESTAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-TRAVPATI     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

           GOBACK.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT  CONTRATI
                       TRAVPATI
               OUTPUT  DESTRAVA
                       RELDESTR.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           PERFORM 1400-OBTER-DATA-CORRENTE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-TESTAR-FS-CONTRATI.

           PERFORM 1220-TESTAR-FS-TRAVPATI.

           PERFORM 1230-TESTAR-FS-DESTRAVA.

           PERFORM 1260-TESTAR-FS-RELDESTR.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-TESTAR-FS-CONTRATI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CONTRATI         NOT EQUAL '00'
               MOVE 'CONTRATI'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CONTRATI    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1220-TESTAR-FS-TRAVPATI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-TRAVPATI         NOT EQUAL '00'
               MOVE 'TRAVPATI'         TO WRK-NOME-ARQ
               MOVE WRK-FS-TRAVPATI    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1230-TESTAR-FS-DESTRAVA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DESTRAVA         NOT EQUAL '00'
               MOVE 'DESTRAVA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-DESTRAVA    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1260-TESTAR-FS-RELDESTR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELDESTR         NOT EQUAL '00'
               MOVE 'RELDESTR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RELDESTR    TO WRK-FILE-STATUS
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

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

           MOVE  WRK-TIMESTAMP-AAAA    TO  CB1-ANO-R2
           MOVE  WRK-TIMESTAMP-MM      TO  CB1-MES-R2
           MOVE  WRK-TIMESTAMP-DD      TO  CB1-DIA-R2
           MOVE  WRK-TIMESTAMP-HH      TO  CB2-HORA-R2
           MOVE  WRK-TIMESTAMP-MN      TO  CB2-MINUTO-R2
           MOVE  WRK-TIMESTAMP-SS      TO  CB2-SEGUNDO-R2.

           MOVE  WRK-TIMESTAMP-DATA    TO WRK-DATA-MOVTO.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-TESTAR-VAZIO               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CONTRATI.

           IF  WRK-FS-CONTRATI   EQUAL '10'
               DISPLAY   '***********BVVE0609******************* '
               DISPLAY   '*                                    * '
               DISPLAY   '*       ARQ    CONTRATI   VAZIO      * '
               DISPLAY   '*                                    * '
               DISPLAY   '************************************** '
           END-IF.

           PERFORM 2200-LER-TRAVPATI.

           IF  WRK-FS-TRAVPATI    EQUAL '10'
               DISPLAY   '***********BVVE0609******************* '
               DISPLAY   '*                                    * '
               DISPLAY   '*       ARQ    TRAVPATI   VAZIO      * '
               DISPLAY   '*                                    * '
               DISPLAY   '************************************** '
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-CONTRATI               SECTION.
      *----------------------------------------------------------------*

           READ CONTRATI               INTO REG-BVVEMM.

           IF  WRK-FS-CONTRATI         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-CONTR
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-CONTRATI.

           MOVE BVVEMM-CNPJ            TO WRK-CCNPJ-CONTR
           MOVE BVVEMM-BAND-SCG        TO WRK-BAND-CONTR
           ADD     1                   TO ACU-LID-CONTRATI.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-TRAVPATI               SECTION.
      *----------------------------------------------------------------*

           READ TRAVPATI  INTO         WRK-DW-TRAVPATI.

           IF  WRK-FS-TRAVPATI         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-TRAV
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO
           PERFORM 1220-TESTAR-FS-TRAVPATI

           MOVE WRK-DW-CH-CNPJ-CPF     TO WRK-CCNPJ-TRAV
           MOVE WRK-DW-COD-PROP-ESQM   TO WRK-BAND-TRAV.
           ADD     1                   TO ACU-LID-TRAVPATI.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-CONTR            EQUAL WRK-CHV-TRAV
               PERFORM 2100-LER-CONTRATI
               UNTIL WRK-CHV-CONTR      NOT EQUAL WRK-CHV-TRAV
               MOVE WRK-CHV-TRAV        TO WRK-CHV-TRAV-ANT
               PERFORM 2200-LER-TRAVPATI
               UNTIL WRK-CHV-TRAV       NOT EQUAL WRK-CHV-TRAV-ANT
           ELSE
               IF  WRK-CHV-CONTR       LESS WRK-CHV-TRAV
                   MOVE WRK-CHV-CONTR  TO WRK-CHV-CONTR-ANT
                   PERFORM 2100-LER-CONTRATI
                   UNTIL WRK-CHV-CONTR NOT EQUAL WRK-CHV-CONTR-ANT
               ELSE
                   MOVE WRK-DW-TIMES-MANUT(1:10) TO WRK-DATA-TIMES-R
                   MOVE CORR WRK-DATA-TIMES      TO WRK-DATA-INV-R
                   IF  WRK-DATA-INV        NOT EQUAL WRK-DATA-MOVTO
                       PERFORM 3300-GRAVAR-DESTRAVA
                       PERFORM 3500-EMITIR-RELDESTR
                       ADD 1           TO ACU-GRA-RELDESTR
                   END-IF
                   PERFORM 2200-LER-TRAVPATI
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-GRAVAR-DESTRAVA            SECTION.
      *----------------------------------------------------------------*

111120     INITIALIZE WRK-BVVEMA-REGISTRO.

111120     MOVE 'ASCG013'              TO WRK-BVVEMA-COD-MSG.
111120     MOVE BVVEMM-CCUSTO          TO WRK-BVVEMA-ROTINA.
      *
      *    IF  WRK-DW-TP-PSSOA         EQUAL 'J' AND
      *        WRK-DW-CH-FILIAL        EQUAL ZEROS
7C2511     IF  WRK-DW-TP-PSSOA         EQUAL 'J' AND
7C2511        (WRK-DW-CH-FILIAL        EQUAL SPACES)
111120         MOVE 'S'                TO WRK-BVVEMA-E-TRAVA-RAIZ
           ELSE
111120         MOVE 'N'                TO WRK-BVVEMA-E-TRAVA-RAIZ
           END-IF.

           IF  WRK-DW-TP-PSSOA           EQUAL 'F'
111120         MOVE WRK-DW-CH-CNPJ-CPF   TO WRK-CPFCNPJ
111120         MOVE ZEROS                TO WRK-FILIAL
111120         MOVE WRK-DW-CTRL-CNPJ-CPF TO WRK-DIGITO
111120         MOVE WRK-CPFCNPJ-NUM      TO WRK-BVVEMA-E-CNPJCPF-CLI
           ELSE
111120         MOVE WRK-DW-CH-CNPJ-CPF   TO WRK-CPFCNPJ
111120         MOVE WRK-DW-CH-FILIAL     TO WRK-FILIAL
111120         MOVE WRK-DW-CTRL-CNPJ-CPF TO WRK-DIGITO
      *        IF   WRK-FILIAL           EQUAL ZEROS
7C2511         IF   WRK-FILIAL           EQUAL SPACES
                    MOVE '0001'          TO WRK-FILIAL
               END-IF
111120         MOVE WRK-CPFCNPJ-NUM      TO WRK-BVVEMA-E-CNPJCPF-CLI
           END-IF.

111120     MOVE WRK-DW-TP-PSSOA         TO WRK-BVVEMA-E-TP-CLI
111120     MOVE WRK-DW-NUM-IDENT-SCG    TO WRK-BVVEMA-E-NUM-IDENTC-SCG
111120     MOVE WRK-DW-CH-COD-BAND      TO WRK-BVVEMA-E-BANDEIRA
111120     MOVE WRK-DW-COD-PROP-ESQM    TO WRK-BVVEMA-E-COD-PROPT-ESQM
111120     MOVE WRK-DATA-MOVTO          TO WRK-BVVEMA-E-DT-MOVTO
111120     MOVE '1'                     TO WRK-BVVEMA-E-TP-BAIXA-DOMCL

111120     WRITE FD-REG-DESTRAVA       FROM   WRK-BVVEMA-REGISTRO
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO

           PERFORM 1230-TESTAR-FS-DESTRAVA

           ADD 1                       TO  ACU-GRA-DESTRAVA.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-EMITIR-RELDESTR             SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              GREATER 57
               PERFORM 3510-IMPRIMIR-CABECALHO
           END-IF.

111120     MOVE WRK-DW-TP-PSSOA             TO LD1-TP-PSSOA-R2
111120     MOVE WRK-DW-CH-CNPJ-CPF          TO LD1-CNPJ-CPF-R2
111120     MOVE WRK-DW-CTRL-CNPJ-CPF        TO LD1-DIG-R2
           MOVE WRK-DW-AG-CLI               TO LD1-AGENCIA-R2
           MOVE WRK-DW-CTA-CLI(6:7)         TO LD1-CONTA-R2
           MOVE WRK-DW-CTA-CLI(13:1)        TO LD1-DIG-CTA
           MOVE WRK-DW-NUM-IDENT-SCG        TO LD1-NUM-SCG-R2
           MOVE WRK-DW-DTINI-VALIDD         TO WRK-DATA-INV
           MOVE CORR WRK-DATA-INV-R         TO WRK-DATA-DB2
           MOVE WRK-DATA-DB2                TO LD1-DT-INI-R2
           MOVE WRK-DW-DTFIM-VALIDD         TO WRK-DATA-INV
           MOVE CORR WRK-DATA-INV-R         TO WRK-DATA-DB2
           MOVE WRK-DATA-DB2                TO LD1-DT-FIM-R2

           EVALUATE WRK-DW-COD-PROP-ESQM
               WHEN '003'
               MOVE 'MASTER'           TO  LD1-BAND-R2
               WHEN '004'
               MOVE 'VISA'             TO  LD1-BAND-R2
               WHEN '005'
               MOVE 'DINERS'           TO  LD1-BAND-R2
               WHEN '006'
               MOVE 'ELO'              TO  LD1-BAND-R2
               WHEN '007'
               MOVE 'AMEX'             TO  LD1-BAND-R2
               WHEN '008'
               MOVE 'HIPER'            TO  LD1-BAND-R2
               WHEN OTHER
               MOVE SPACES             TO  LD1-BAND-R2
           END-EVALUATE.

           WRITE FD-REG-RELDESTR       FROM WRK-LINDET1-R2
           PERFORM 1260-TESTAR-FS-RELDESTR.

           ADD 1                       TO ACU-LINHAS

           MOVE SPACES                 TO WRK-LINDET1-R2 .

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3510-IMPRIMIR-CABECALHO         SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS

           MOVE ACU-PAGINAS            TO CB1-PAGINA-R2
           WRITE FD-REG-RELDESTR       FROM WRK-CABEC1-R2
           PERFORM 1260-TESTAR-FS-RELDESTR

           WRITE FD-REG-RELDESTR       FROM WRK-CABEC2-R2
           PERFORM 1260-TESTAR-FS-RELDESTR

           WRITE FD-REG-RELDESTR       FROM WRK-CABEC3-R2
           PERFORM 1260-TESTAR-FS-RELDESTR

           WRITE FD-REG-RELDESTR       FROM WRK-CABEC4-R2
           PERFORM 1260-TESTAR-FS-RELDESTR
           MOVE 6                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE CONTRATI
                 TRAVPATI
                 DESTRAVA
                 RELDESTR.

           MOVE WRK-FECHAMENTO         TO  WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           GOBACK.
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** BVVE0609 ******************'
           DISPLAY '*                                         *'

           MOVE ACU-LID-CONTRATI      TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS CONTRATI: ' WRK-MASCARA '      *'

           MOVE ACU-LID-TRAVPATI      TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS TRAVPATI: ' WRK-MASCARA '      *'

           MOVE ACU-GRA-DESTRAVA     TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA DESTRAVA: ' WRK-MASCARA '      *'

           MOVE ACU-GRA-RELDESTR      TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA RELDESTR: ' WRK-MASCARA '      *'
           DISPLAY '*                                         *'

           DISPLAY '*************** BVVE0609 ******************'.

           IF  ACU-GRA-RELDESTR        GREATER ZEROS
               MOVE ACU-GRA-RELDESTR   TO WRK-LT1-TOTAL-R2
               WRITE FD-REG-RELDESTR   FROM WRK-LINTOT1-R2
               PERFORM 1260-TESTAR-FS-RELDESTR
           END-IF.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE0609'             TO ERR-PGM.

           CALL 'BRAD7100'         USING WRK-BATCH
                                         ERRO-AREA.

           GOBACK.
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
