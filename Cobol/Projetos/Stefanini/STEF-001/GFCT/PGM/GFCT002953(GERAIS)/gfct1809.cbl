      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1809.
       AUTHOR.     ANDRE LUIZ LONGO.
      *================================================================*
      *.                   C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1809                                    *
      *    PROGRAMADOR.:   ANDRE LUIZ LONGO         - CPM PATO BRANCO  *
      *    ANALISTA CPM:   KHARUZO I. LEITE         - CPM PATO BRANCO  *
      *    ANALISTA....:   WAGNER SILVA             - PROCWORK / GP.50 *
      *    DATA........:   29/09/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   APLICA REGRAS PARA APURACAO DE CARENCIA.    *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    VERCAREN                  I#GFCTVE          *
      *                    FRANQT92                  I#GFCTVA          *
      *                    FRANQT41                  I#GFCTV4          *
      *                    SEGCOBRA                  I#GFCTVA          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *                    DB2PRD.V01CTA_POUPANCA        CLIEV007      *
      *                    DB2PRD.V01CTA_CORRENTE        CLIEV008      *
      *                    DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - CONEXAO COM DB2.                                 *
      *    POOL1285 - SOMAR OU SUBTRAIR UM NUMERO DE DIAS EM UMA DATA. *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
      *    POOL7600 - OBTER DATA/HORA ATUAL DO SISTEMA.                *
      *================================================================*
      *    PACOTE DE 24/11/2006                                        *
      *    MOTIVO: STAGE PARA PEGAR A ALTERACAO DA INC I#GFCTVA COM    *
      *            OS CAMPOS DE AGRUPAMENTO                            *
      *================================================================*
      *================================================================*
      *    ULTIMA ALTERACAO: MAR/2007 - VALERIA                        *
      *    ATUALIZACAO DA INC I#GFCTVA                                 *
      *================================================================*
0208  *================================================================*
0208  *                     A L T E R A C A O                          *
0208  *----------------------------------------------------------------*
0208  *    ANALISTA....:   WAGNER                - PROCWORK - GRUPO 50 *
0208  *    DATA........:   12/02/2008                                  *
0208  *    OBJETIVO....:   ATUALIZACAO DA INC I#GFCTVE                 *
0208  *================================================================*
280409*================================================================*
280409*      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
280409*----------------------------------------------------------------*
280409*    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
280409*    ANALISTA....:   WAGNER SILVA                - SONDPROC/GP.50*
280409*    DATA........:   28/04/2009                                  *
280409*                                                                *
280409*    OBJETIVO    :   RETIRAR TRATAMENTOS E MUDAR BOOK DE SAIDA.  *
280409*================================================================*

280609*================================================================*
190609*      S O N D A  P R O C W O R K  -  ALOCACAO BRADESCO          *
190609*----------------------------------------------------------------*
190609*    ANALISTA....:   WAGNER SILVA               - SONDPROC/GP.50 *
190609*    DATA........:   28/04/2009                                  *
190609*    OBJETIVO    :   RETIRAR DISPLAY DE VE-CINDCD-INIC-CAREN = 0 *
190609*                    MOVIMENTAR ENTRADA PARA SAIDA VA            *
190609*================================================================*

      *===============================================================*
      *                   M A N U T E N C A O                         *
      *===============================================================*
      *---------------------------------------------------------------*
      *    OBJETIVO....:   ALTERACAO DO TAMANHO DA COLUNA DA TABELA   *
      *     (JC0413)       GFCTBA2 - QDIA_CAREN_TARIF DE 9(02) PARA   *
      *                    9(03) - ALTERCAO TAMANHO DO CAMPO NA INC.  *
      *                    I#GFCTVE (LAYOUT DO ARQUIVO DE ENTRADA).   *
      *    OBS.........:   APENAS RECOMPILADO (INC.I#GFCTVE).         *
      *---------------------------------------------------------------*
      *    PROGRAMADOR.:   J.C.R.                    - SONDA IT       *
      *    ANALISTA DDS:   HELENA                    - GP.50          *
      *    ANALISTA....:   PAGNNOCA                  - SONDA IT       *
      *    DATA........:   ABRIL/2013                                 *
      *---------------------------------------------------------------*
      *===============================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT VERCAREN ASSIGN      TO UT-S-VERCAREN
           FILE STATUS                 IS WRK-FS-VERCAREN.

           SELECT FRANQT92 ASSIGN      TO UT-S-FRANQT92
           FILE STATUS                 IS WRK-FS-FRANQT92.

           SELECT FRANQT41 ASSIGN      TO UT-S-FRANQT41
           FILE STATUS                 IS WRK-FS-FRANQT41.

           SELECT SEGCOBRA ASSIGN      TO UT-S-SEGCOBRA
           FILE STATUS                 IS WRK-FS-SEGCOBRA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : REGISTROS A COBRAR.                                 *
      *            ORG. SEQUENCIAL     -   LRECL = 300                 *
      *----------------------------------------------------------------*

       FD  VERCAREN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTVE'.

      *----------------------------------------------------------------*
      *    OUTPUT: FRANQUEADOS PARA GRAVAR NA TABELA GFCTB092.         *
      *            ORG. SEQUENCIAL     -   LRECL = 250                 *
      *----------------------------------------------------------------*

       FD  FRANQT92
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-FRANQT92                PIC  X(250).

      *----------------------------------------------------------------*
      *    OUTPUT: FRANQUEADOS PARA GRAVAR NA TABELA GFCTB041.         *
      *            ORG. SEQUENCIAL     -   LRECL = 150                 *
      *----------------------------------------------------------------*

       FD  FRANQT41
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTV4'.

      *----------------------------------------------------------------*
      *    OUTPUT: SEGUEM PARA CONTINUAR PROCESSO DE COBRANCA.         *
      *            ORG. SEQUENCIAL     -   LRECL = 300                 *
      *----------------------------------------------------------------*

       FD  SEGCOBRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

280409 COPY 'I#GFCTVE'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-DESPREZADOS             PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-VERCAREN          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-FRANQT92          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-FRANQT41          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SEGCOBRA          PIC  9(009) COMP-3  VALUE ZEROS.
RET    77  WRK-POOL1205-OC             PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-VERCAREN             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FRANQT92             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FRANQT41             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SEGCOBRA             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC  ZZZZ.ZZZ.999   VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-ERR-MSG                 PIC  X(042)         VALUE SPACES.
       77  WRK-ERR-CHAVE               PIC  X(040)         VALUE SPACES.

       01  WRK-VALOR                   PIC  9(009)V9(04) COMP-3
                                                           VALUE ZEROS.
       01  WRK-PERCENTUAL              PIC  9(003)V9(04) COMP-3
                                                           VALUE ZEROS.
       01  WRK-VLR-T41                 PIC  9(009)V9(02) COMP-3
                                                           VALUE ZEROS.
       01  WRK-AG-CTA.
           05  WRK-AGENCIA             PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(003)         VALUE ' - '.
           05  WRK-CONTA               PIC  9(013)         VALUE ZEROS.

       01  WRK-DATA-DB2.
           05 WRK-DIA-DB2              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-MES-DB2              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-ANO-DB2              PIC  9(004)         VALUE ZEROS.

       01  WRK-DPROCM                  PIC  9(008)         VALUE ZEROS.
       01  WRK-DPROCM-R                REDEFINES           WRK-DPROCM
                                       PIC  X(008).

       01  WRK-DFINAL                  PIC  9(008)         VALUE ZEROS.
       01  WRK-DFINAL-R                REDEFINES           WRK-DFINAL
                                       PIC  X(008).

       01  WRK-DATA-AGEND.
           05 WRK-ANO-AGEND            PIC  9(004)         VALUE ZEROS.
           05 WRK-MES-AGEND            PIC  9(002)         VALUE ZEROS.
           05 WRK-DIA-AGEND            PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-AUX                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-AUX-X              REDEFINES           WRK-DATA-AUX
                                       PIC  X(008).

       01  WRK-DATA-AUX2               PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-AUX2-X             REDEFINES          WRK-DATA-AUX2.
           05  WRK-ANO-AUX2            PIC  9(004).
           05  WRK-MES-AUX2            PIC  9(002).
           05  WRK-DIA-AUX2            PIC  9(002).

       01  WRK-PER-INI                 PIC  9(008)         VALUE ZEROS.
       01  WRK-PER-INI-X               REDEFINES WRK-PER-INI.
           05 WRK-ANO-PER-INI          PIC  9(004).
           05 WRK-MES-PER-INI          PIC  9(002).
           05 WRK-DIA-PER-INI          PIC  9(002).

       01  WRK-PER-FIM                 PIC  9(008)         VALUE ZEROS.
       01  WRK-PER-FIM-X               REDEFINES WRK-PER-FIM.
           05 WRK-ANO-PER-FIM          PIC  9(004).
           05 WRK-MES-PER-FIM          PIC  9(002).
           05 WRK-DIA-PER-FIM          PIC  9(002).

       01  WRK-TIMESTAMP-AUX.
           05 WRK-ANO-TMS              PIC  9(004)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-MES-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-DIA-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-HOR-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MIN-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-SEG-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MIL-TMS              PIC  9(006)         VALUE ZEROS.

       01  WRK-X-40                    PIC  X(040)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-X-40.
           05  WRK-X-5                 PIC  X(005).
           05  FILLER                  PIC  X(035).

       01  WRK-S9-5                    PIC S9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5.
           05  WRK-9-5                 PIC  9(005).

       01  WRK-9-9-9                   PIC  9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-9-9-9.
           05  WRK-9-5-X               PIC  X(005).

       01  WRK-S9-3                    PIC S9(003)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-3.
           05  WRK-9-3                 PIC  9(003).

       01  WRK-S9-13                   PIC S9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-13.
           05  WRK-9-13                PIC  9(013).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CHAVES *'.
      *----------------------------------------------------------------*

       01  WRK-CHV-ATU.
           05 WRK-CHV-ATU-CLI.
             10 WRK-CHV-ATU-BANCO      PIC  9(003)         VALUE ZEROS.
             10 WRK-CHV-ATU-AGENCIA    PIC  9(005)         VALUE ZEROS.
             10 WRK-CHV-ATU-CONTA      PIC  9(013)         VALUE ZEROS.
           05 WRK-CHV-ATU-DADOS.
             10 WRK-CHV-ATU-TARIFA     PIC  9(005)         VALUE ZEROS.
             10 WRK-CHV-ATU-DT-OCOR    PIC  9(008)         VALUE ZEROS.
             10 WRK-CHV-ATU-DT-OCOR-X  REDEFINES WRK-CHV-ATU-DT-OCOR
                                       PIC  X(008).

       01  WRK-CHV-ANT.
           05 WRK-CHV-ANT-CLI.
             10 WRK-CHV-ANT-BANCO      PIC  9(003)         VALUE ZEROS.
             10 WRK-CHV-ANT-AGENCIA    PIC  9(005)         VALUE ZEROS.
             10 WRK-CHV-ANT-CONTA      PIC  9(013)         VALUE ZEROS.
           05 WRK-CHV-ANT-DADOS.
             10 WRK-CHV-ANT-TARIFA     PIC  9(005)         VALUE ZEROS.
             10 WRK-CHV-ANT-DT-OCOR    PIC  9(008)         VALUE ZEROS.
             10 WRK-CHV-ANT-DT-OCOR-X  REDEFINES WRK-CHV-ANT-DT-OCOR
                                       PIC  X(008).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA POOL1285'.
      *----------------------------------------------------------------*

       01  WRK-MSG001                  PIC  X(030)         VALUE
           '* ERRO NO ACESSO A POOL1285 *'.

       01  WRK-AREA-POOL1285.
           05  WRK-DT-INFERIOR-POOL    PIC  9(008) COMP-3  VALUE ZEROS.
           05  WRK-QTDE-DIAS-1285      PIC S9(005) COMP-3  VALUE ZEROS.
           05  WRK-DT-SUPERIOR-POOL    PIC  9(008) COMP-3  VALUE ZEROS.
           05  WRK-MENSAGEM            PIC  X(050)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DA POOL7600'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05 WRK-DATA-JULIANA         PIC  9(005) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAMMDD          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAAAMMDD        PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSS          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSSMMMMMM    PIC  9(013) COMP-3  VALUE ZEROS.
           05 WRK-TIMESTAMP.
             10 WRK-ANO                PIC  9(004)         VALUE ZEROS.
             10 WRK-MES                PIC  9(002)         VALUE ZEROS.
             10 WRK-DIA                PIC  9(002)         VALUE ZEROS.
             10 WRK-HOR                PIC  9(002)         VALUE ZEROS.
             10 WRK-MIN                PIC  9(002)         VALUE ZEROS.
             10 WRK-SEG                PIC  9(002)         VALUE ZEROS.
             10 WRK-MIL                PIC  9(006)         VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* MENSAGEM DE ERRO DE FILE-STATUS *'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ARQ. SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTVA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM PROCESSADAS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-VERCAREN     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS INICIAIS E BUSCAR DATA DE PROCESSAMENTO.         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  VERCAREN
                OUTPUT FRANQT92
                       FRANQT41
                       SEGCOBRA.

           PERFORM 1010-DATA-PROCESSAMENTO

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           MOVE WRK-ANO                TO WRK-ANO-TMS.
           MOVE WRK-MES                TO WRK-MES-TMS.
           MOVE WRK-DIA                TO WRK-DIA-TMS.
           MOVE WRK-HOR                TO WRK-HOR-TMS.
           MOVE WRK-MIN                TO WRK-MIN-TMS.
           MOVE WRK-SEG                TO WRK-SEG-TMS.
           MOVE WRK-MIL                TO WRK-MIL-TMS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    BUSCAR DATA PROCESSAMENTO.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1010-DATA-PROCESSAMENTO         SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-DB2.

           STRING WRK-ANO-DB2 WRK-MES-DB2 WRK-DIA-DB2
           DELIMITED BY SIZE           INTO WRK-DPROCM-R.

      *----------------------------------------------------------------*
       1010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DOS ARQUIVOS.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-VERCAREN.

           PERFORM 1120-TESTAR-FS-FRANQT92.

           PERFORM 1130-TESTAR-FS-FRANQT41.

           PERFORM 1140-TESTAR-FS-SEGCOBRA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO VERCAREN.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-VERCAREN         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-VERCAREN         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-VERCAREN    TO WRK-FILE-STATUS
               MOVE 'VERCAREN'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO FRANQT92.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-FRANQT92         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FRANQT92         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-FRANQT92    TO WRK-FILE-STATUS
               MOVE 'FRANQT92'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO FRANQT41.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-FRANQT41         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FRANQT41         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-FRANQT41    TO WRK-FILE-STATUS
               MOVE 'FRANQT41'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO SEGCOBRA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-SEGCOBRA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEGCOBRA         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-SEGCOBRA    TO WRK-FILE-STATUS
               MOVE 'SEGCOBRA'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE ARQUIVO ESTA VAZIO.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-VERCAREN.

           IF WRK-FS-VERCAREN          EQUAL '10'
              DISPLAY '******************** GFCT1809 ******************'
              DISPLAY '*                                              *'
              DISPLAY '*          ARQUIVO DE VERCAREN VAZIO           *'
              DISPLAY '*                                              *'
              DISPLAY '******************** GFCT1809 ******************'
              MOVE 04                  TO RETURN-CODE
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO VERCAREN                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-VERCAREN               SECTION.
      *----------------------------------------------------------------*

           READ VERCAREN.

           IF  WRK-FS-VERCAREN         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-VERCAREN.

           MOVE VE-CBCO-DSTNO-MOVTO OF VERCAREN TO WRK-S9-3.
           MOVE WRK-9-3                         TO WRK-CHV-ATU-BANCO.
           MOVE VE-CAG-DSTNO-MOVTO  OF VERCAREN TO WRK-S9-5.
           MOVE WRK-9-5                         TO WRK-CHV-ATU-AGENCIA.
           MOVE VE-CCTA-DSTNO-MOVTO OF VERCAREN TO WRK-S9-13.
           MOVE WRK-9-13                        TO WRK-CHV-ATU-CONTA.
           MOVE VE-CSERVC-TARIF     OF VERCAREN TO WRK-S9-5.
           MOVE WRK-9-5                         TO WRK-CHV-ATU-TARIFA.
           MOVE VE-DOCOR-EVNTO      OF VERCAREN TO WRK-DATA-DB2.

           STRING WRK-ANO-DB2 WRK-MES-DB2 WRK-DIA-DB2
           DELIMITED BY SIZE           INTO WRK-CHV-ATU-DT-OCOR-X.

           ADD 1                       TO ACU-LIDOS-VERCAREN.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

VINI  *    XISPLAY '                             '.
VINI  *    XISPLAY 'REINICIO...CHV.VERCAREN.....:' WRK-CHV-ATU.
VINI  *    XISPLAY 'REG / DOCOR / DAGEND' VE-CSEQ-MOVTO OF VERCAREN
VINI  *            ' / ' VE-DOCOR-EVNTO OF VERCAREN
      *            ' / ' VE-DAGNDA-DEB-MOVTO OF VERCAREN.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

VINI  *    XISPLAY 'TIPO DE CARENCIA....'
      *                VE-CINDCD-INIC-CAREN OF VERCAREN.

           EVALUATE VE-CINDCD-INIC-CAREN OF VERCAREN
               WHEN 1
                   PERFORM 3100-TRATAR-INIC-CARENCIA
               WHEN 3
                   PERFORM 3200-TRATAR-INIC-VIGENCIA
280409         WHEN 4
                   PERFORM 3300-TRATAR-ABERTURA-CONTA
280409         WHEN OTHER
280409             MOVE REG-IGFCTVE OF VERCAREN
280409                              TO REG-IGFCTVE OF SEGCOBRA
280619             MOVE VE-GFCTB092 OF VERCAREN
280619                              TO GFCTVA-GFCTB092
280409             ADD  1           TO ACU-DESPREZADOS
280409             GO TO 3000-GRAVAR-SEGCOBRA
280409     END-EVALUATE.

           MOVE VE-GFCTB092     OF VERCAREN TO GFCTVA-GFCTB092.
280409     MOVE REG-IGFCTVE     OF VERCAREN TO REG-IGFCTVE OF SEGCOBRA.
           MOVE VE-CTRATO-MOVTO OF VERCAREN TO GFCTVA-CTRATO-MOVTO.
           MOVE ZEROS                       TO GFCTVA-QTD-FQ-COMPON
                                               GFCTVA-QTD-EXC-DIA
                                               GFCTVA-QTD-EXC-ACU
                                               GFCTVA-AGENCIA-CREC
                                               GFCTVA-CONTA-CREC
                                               GFCTVA-CSGMTO-GSTAO-TARIF
                                               GFCTVA-PACOTE-EVENTO.
           MOVE SPACES                      TO GFCTVA-FLAG-DATA.

           IF WRK-ERR-MSG             NOT EQUAL SPACES
              DISPLAY '********************** GFCT1809 *****************
      -               '***'
              DISPLAY '* ERRO NO ACESSO DB2 - REGISTRO NAO PROCESSADO
      -               '  *'
              DISPLAY '* ' WRK-ERR-MSG '       *'
              DISPLAY '* CHAVE = ' WRK-ERR-CHAVE ' *'
              DISPLAY '******************** GFCT1809 *******************
      -               '***'
               MOVE SPACES             TO WRK-ERR-MSG
               ADD 1                   TO ACU-DESPREZADOS
               GO TO 3000-GRAVAR-SEGCOBRA
           END-IF.

           MOVE VE-DAGNDA-DEB-MOVTO OF VERCAREN TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2                     TO WRK-DIA-AGEND.
           MOVE WRK-MES-DB2                     TO WRK-MES-AGEND.
           MOVE WRK-ANO-DB2                     TO WRK-ANO-AGEND.

           IF (WRK-CHV-ATU-DT-OCOR  NOT LESS    WRK-PER-INI  AND
               WRK-CHV-ATU-DT-OCOR  NOT GREATER WRK-PER-FIM) AND
               VE-PDESC-CAREN-TARIF OF VERCAREN NOT EQUAL ZEROS
               PERFORM 3400-CALCULAR-VLR-COBRAR
               IF  GFCTVA-VTARIF-LIQ-MOVTO EQUAL ZEROS
                   PERFORM 3500-GRAVAR-FRANQUEADOS
                   PERFORM 3600-GRAVAR-FRANQUIA41
                   PERFORM 2100-LER-VERCAREN
                   GO TO 3000-99-FIM
               END-IF
           END-IF.

           IF  VE-VTARIF-LIQ-MOVTO OF VERCAREN
                                   NOT EQUAL GFCTVA-VTARIF-LIQ-MOVTO
               PERFORM 3600-GRAVAR-FRANQUIA41
VINI  *        XISPLAY 'GRAVOU FRANQT41     '
           END-IF.

       3000-GRAVAR-SEGCOBRA.

           PERFORM 3900-GRAVAR-SEGCOBRA.

VINI  *    XISPLAY 'GRAVOU SEGCOBRA     '

           PERFORM 2100-LER-VERCAREN.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR INICIO CARENCIA                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-TRATAR-INIC-CARENCIA       SECTION.
      *----------------------------------------------------------------*

VINI  *    XISPLAY 'INICIO CARENCIA     '.

           IF  WRK-CHV-ATU             EQUAL WRK-CHV-ANT
VINI  *        XISPLAY 'CHAVE IGUAL'
               GO TO 3100-99-FIM
           END-IF.

           MOVE VE-DINIC-CAREN-TARIF OF VERCAREN TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2                      TO WRK-DIA-PER-INI
                                                    WRK-DIA-PER-FIM.
           MOVE WRK-MES-DB2                      TO WRK-MES-PER-INI
                                                    WRK-MES-PER-FIM.
           MOVE WRK-ANO-DB2                      TO WRK-ANO-PER-INI
                                                    WRK-ANO-PER-FIM.

           MOVE WRK-PER-FIM                      TO WRK-DATA-AUX.
JC0413     MOVE VE-QDIA-CAREN-TARIF OF VERCAREN  TO WRK-QTDE-DIAS-1285.

           PERFORM 3700-CALCULAR-DATA.

           MOVE WRK-DATA-AUX           TO WRK-PER-FIM.
           MOVE WRK-CHV-ATU            TO WRK-CHV-ANT.

VINI  *    XISPLAY 'VE-DINIC-CAREN-TARIF.' VE-DINIC-CAREN-TARIF
VINI  *                                                    OF VERCAREN.
VINI  *    XISPLAY 'TARIF-QDIA/INI/FIM.' VE-QDIA-CAREN-TARIF
VINI  *                OF VERCAREN ' / ' WRK-PER-INI ' / ' WRK-PER-FIM.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR INICIO VIGENCIA                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-TRATAR-INIC-VIGENCIA       SECTION.
      *----------------------------------------------------------------*

VINI  *    XISPLAY 'INICIO VIGENCIA     '.

           IF  WRK-CHV-ATU             EQUAL WRK-CHV-ANT
VINI  *        XISPLAY 'CHAVE IGUAL'
               GO TO 3200-99-FIM
           END-IF.

           MOVE VE-CSERVC-TARIF OF VERCAREN
                                TO CSERVC-TARIF OF GFCTB0A2.

           EXEC SQL
             SELECT MIN(DINIC_VGCIA_TARIF)
             INTO
                   :GFCTB0A2.DINIC-VGCIA-TARIF
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF  (SQLCODE                    NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                   EQUAL 'W')
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                MOVE 'PARM_SERVC_TARIF  '  TO ERR-DBD-TAB
                MOVE 'SELECT    '          TO ERR-FUN-COMANDO
                MOVE SQLCODE               TO ERR-SQL-CODE
                MOVE '0010'                TO ERR-LOCAL
                MOVE SPACES                TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'REGISTRO NAO ENCONTRADO NA TABELA GFCTB0A2'
                                                TO WRK-ERR-MSG
               MOVE VE-CSERVC-TARIF OF VERCAREN TO WRK-S9-5
               MOVE WRK-9-5                     TO WRK-9-9-9
               MOVE WRK-9-5-X                   TO WRK-X-5
               MOVE WRK-X-40                    TO WRK-ERR-CHAVE
               GO TO 3200-99-FIM
           ELSE
               MOVE DINIC-VGCIA-TARIF  OF GFCTB0A2
                                       TO WRK-DATA-DB2
               MOVE WRK-DIA-DB2        TO WRK-DIA-PER-INI
                                          WRK-DIA-PER-FIM
               MOVE WRK-MES-DB2        TO WRK-MES-PER-INI
                                          WRK-MES-PER-FIM
               MOVE WRK-ANO-DB2        TO WRK-ANO-PER-INI
                                          WRK-ANO-PER-FIM
           END-IF.

           MOVE WRK-PER-FIM                     TO WRK-DATA-AUX.
JC0413     MOVE VE-QDIA-CAREN-TARIF OF VERCAREN TO WRK-QTDE-DIAS-1285.

           PERFORM 3700-CALCULAR-DATA.

           MOVE WRK-DATA-AUX           TO WRK-PER-FIM.
           MOVE WRK-CHV-ATU            TO WRK-CHV-ANT.

VINI  *    XISPLAY 'DATA INICIO VIGENCIA. ' WRK-PER-INI.
VINI  *    XISPLAY 'TARIF- QDIA/INI/FIM'
VINI  *    VE-QDIA-CAREN-TARIF OF VERCAREN ' / '
VINI  *             WRK-PER-INI ' / ' WRK-PER-FIM.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR ABERTURA DE CONTA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-TRATAR-ABERTURA-CONTA      SECTION.
      *----------------------------------------------------------------*

VINI  *    XISPLAY 'ABERTURA CONTA      '.

           IF  WRK-CHV-ATU             EQUAL WRK-CHV-ANT
VINI  *        XISPLAY 'CHAVE IGUAL'
               GO TO 3300-99-FIM
           END-IF.

           MOVE VE-CSERVC-TARIF OF VERCAREN
                                TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT
                   CTPO_SERVC_TARIF ,
RODRIG             CTPO_CTA_DEB_TARIF
             INTO
                   :GFCTB0D8.CTPO-SERVC-TARIF ,
RODRIG             :GFCTB0D8.CTPO-CTA-DEB-TARIF
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF  (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                EQUAL 'W')
                MOVE 'DB2'              TO ERR-TIPO-ACESSO
                MOVE 'PARM_SERVC_TARIF' TO ERR-DBD-TAB
                MOVE 'SELECT    '       TO ERR-FUN-COMANDO
                MOVE SQLCODE            TO ERR-SQL-CODE
                MOVE '0020'             TO ERR-LOCAL
                MOVE SPACES             TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'REGISTRO NAO ENCONTRADO NA TABELA GFCTB0A2'
                                                TO WRK-ERR-MSG
               MOVE VE-CSERVC-TARIF OF VERCAREN TO WRK-S9-5
               MOVE WRK-9-5                     TO WRK-9-9-9
               MOVE WRK-9-5-X                   TO WRK-X-5
               MOVE WRK-X-40                    TO WRK-ERR-CHAVE
               GO TO 3300-99-FIM
           END-IF.

WS         IF  VE-CCTA-DSTNO-MOVTO OF VERCAREN GREATER 1000000
               PERFORM 3320-BUSCA-DATA-POUPANCA
           ELSE
               PERFORM 3310-BUSCA-DT-CTA-CORRENTE
           END-IF.

           IF  WRK-ERR-MSG             EQUAL SPACES
RRP        AND WRK-PER-INI             NOT EQUAL        '00010101'
181108     AND WRK-PER-INI             NOT EQUAL        '10010101'
               MOVE WRK-PER-FIM         TO WRK-DATA-AUX
JC0413         MOVE VE-QDIA-CAREN-TARIF OF VERCAREN
                                        TO WRK-QTDE-DIAS-1285
RET            MOVE    05               TO WRK-POOL1205-OC
               PERFORM 3700-CALCULAR-DATA
               MOVE WRK-DATA-AUX        TO WRK-PER-FIM
           END-IF.

           MOVE WRK-CHV-ATU            TO WRK-CHV-ANT.

VINI  *    XISPLAY 'DATA ABERTURA DE CONTA.' WRK-PER-INI.
VINI  *    XISPLAY 'ABERT- QDIA/INI/FIM.'
VINI  *            VE-QDIA-CAREN-TARIF OF VERCAREN ' / '
VINI  *            WRK-PER-INI ' / ' WRK-PER-FIM.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    BUSCA DATA DA CONTA CORRENTE                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-BUSCA-DT-CTA-CORRENTE      SECTION.
      *----------------------------------------------------------------*

           MOVE VE-CAG-DSTNO-MOVTO  OF VERCAREN
                                    TO CJUNC-DEPDC  OF CLIEV008.
           MOVE VE-CCTA-DSTNO-MOVTO OF VERCAREN
                                    TO CCTA-CLI     OF CLIEV008.

           EXEC SQL
             SELECT
                   DABERT_CTA
             INTO
                   :CLIEV008.DABERT-CTA
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE
                   CJUNC_DEPDC        = :CLIEV008.CJUNC-DEPDC        AND
                   CCTA_CLI           = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'CTA_CORRENTE'    TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0030'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'REGISTRO NAO ENCONTRADO NA TABELA CLIEV008'
                                                    TO WRK-ERR-MSG
               MOVE VE-CAG-DSTNO-MOVTO  OF VERCAREN TO WRK-S9-5
               MOVE WRK-9-5                         TO WRK-AGENCIA
               MOVE VE-CCTA-DSTNO-MOVTO OF VERCAREN TO WRK-S9-13
               MOVE WRK-9-13                        TO WRK-CONTA
               MOVE WRK-AG-CTA                      TO WRK-ERR-CHAVE
               GO TO 3310-99-FIM
           ELSE
               MOVE DABERT-CTA         OF CLIEV008
                                       TO WRK-DATA-DB2
               MOVE WRK-DIA-DB2        TO WRK-DIA-PER-INI
                                          WRK-DIA-PER-FIM
               MOVE WRK-MES-DB2        TO WRK-MES-PER-INI
                                          WRK-MES-PER-FIM
               MOVE WRK-ANO-DB2        TO WRK-ANO-PER-INI
                                          WRK-ANO-PER-FIM
           END-IF.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    BUSCA DATA DA CONTA POUPANCA                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3320-BUSCA-DATA-POUPANCA        SECTION.
      *----------------------------------------------------------------*

           MOVE VE-CAG-DSTNO-MOVTO  OF VERCAREN
                                    TO CJUNC-DEPDC  OF CLIEV007.
           MOVE VE-CCTA-DSTNO-MOVTO OF VERCAREN
                                    TO CCTA-CLI     OF CLIEV007.

           EXEC SQL
             SELECT
                   DABERT_CTA
             INTO
                   :CLIEV007.DABERT-CTA
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE
                   CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC        AND
                   CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'CTA_POUPANCA'    TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0040'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'REGISTRO NAO ENCONTRADO NA TABELA CLIEV007'
                                                    TO WRK-ERR-MSG
               MOVE VE-CAG-DSTNO-MOVTO  OF VERCAREN TO WRK-S9-5
               MOVE WRK-9-5                         TO WRK-AGENCIA
               MOVE VE-CCTA-DSTNO-MOVTO OF VERCAREN TO WRK-S9-13
               MOVE WRK-9-13                        TO WRK-CONTA
               MOVE WRK-AG-CTA                      TO WRK-ERR-CHAVE
               GO TO 3320-99-FIM
           ELSE
               MOVE DABERT-CTA         OF CLIEV007
                                       TO WRK-DATA-DB2
               MOVE WRK-DIA-DB2        TO WRK-DIA-PER-INI
                                          WRK-DIA-PER-FIM
               MOVE WRK-MES-DB2        TO WRK-MES-PER-INI
                                          WRK-MES-PER-FIM
               MOVE WRK-ANO-DB2        TO WRK-ANO-PER-INI
                                          WRK-ANO-PER-FIM
           END-IF.

      *----------------------------------------------------------------*
       3320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CALCULAR VALOR A COBRAR                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-CALCULAR-VLR-COBRAR        SECTION.
      *----------------------------------------------------------------*

           COMPUTE WRK-PERCENTUAL = 100 - VE-PDESC-CAREN-TARIF
                                                       OF VERCAREN.

VINI  *    XISPLAY 'PDESC / PDESC CALC'
VINI  *    VE-PDESC-CAREN-TARIF OF VERCAREN ' / ' WRK-PERCENTUAL

           IF  WRK-PERCENTUAL           EQUAL ZEROS
               MOVE GFCTVA-VTARIF-LIQ-MOVTO TO WRK-VLR-T41
               MOVE ZEROS            TO GFCTVA-VTARIF-LIQ-MOVTO

VINI  *        XISPLAY 'PERC ZERADO    ' WRK-VLR-T41 '/'
VINI  *                                  GFCTVA-VTARIF-LIQ-MOVTO
           ELSE

               COMPUTE WRK-VALOR =
                       (VE-VTARIF-LIQ-MOVTO OF VERCAREN / 100)
               COMPUTE GFCTVA-VTARIF-LIQ-MOVTO =
                                            (WRK-VALOR * WRK-PERCENTUAL)

               COMPUTE WRK-VLR-T41 =
               (VE-VTARIF-LIQ-MOVTO OF VERCAREN
                                           - GFCTVA-VTARIF-LIQ-MOVTO)

VINI  *        XISPLAY 'VTARIF-LIQ / 100  ' WRK-VALOR
VINI  *        XISPLAY 'VLR P/ 92         ' WRK-VALOR ' * '
VINI  *                 WRK-PERCENTUAL    ' = ' GFCTVA-VTARIF-LIQ-MOVTO
VINI  *        XISPLAY 'VLR P/ 41         '
VINI  *        VE-VTARIF-LIQ-MOVTO OF VERCAREN ' - '
VINI  *                 GFCTVA-VTARIF-LIQ-MOVTO ' = ' WRK-VLR-T41

           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR FRANQUEADOS                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-GRAVAR-FRANQUEADOS         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTVA-VTARIF-LIQ-MOVTO   = ZEROS
               MOVE 16                  TO GFCTVA-CSIT-EVNTO-RECBD
               MOVE ZEROS               TO GFCTVA-VTARIF-LIQ-MOVTO
               MOVE VE-DAGNDA-DEB-MOVTO OF VERCAREN
                                        TO GFCTVA-DEFETV-DEB-MOVTO
           END-IF.

           WRITE REG-FRANQT92         FROM GFCTVA-GFCTB092.

           PERFORM 1120-TESTAR-FS-FRANQT92.

           ADD 1                        TO ACU-GRAVA-FRANQT92.

VINI  *    XISPLAY 'VLR GRAVADO FRANQ92.' GFCTVA-VTARIF-LIQ-MOVTO.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR FRANQUIA 41                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-GRAVAR-FRANQUIA41          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTV4-GFCTB041.

           INITIALIZE GFCTV4-GFCTB041.

           MOVE 4                      TO GFCTV4-CIDTFD-TPO-DESC.
           MOVE 2                      TO GFCTV4-CIDTFD-MOTVO-DESC.
           MOVE ZEROS                  TO GFCTV4-CIDTFD-DESC.
           MOVE 1                      TO GFCTV4-CPARM-DESC.
           MOVE VE-QEVNTO-AGRUP        OF VERCAREN
                                       TO GFCTV4-QEVNTO-DESC.
           MOVE WRK-VLR-T41            TO GFCTV4-VCONCS-DESC.
           MOVE SPACES                 TO GFCTV4-CCONTR-FLEXZ.
           MOVE VE-DENVIO-MOVTO-TARIF  OF VERCAREN
                                       TO GFCTV4-DENVIO-MOVTO-TARIF.
           MOVE VE-CROTNA-ORIGE-MOVTO  OF VERCAREN
                                       TO GFCTV4-CROTNA-ORIGE-MOVTO.
           MOVE VE-CNRO-ARQ-MOVTO      OF VERCAREN
                                       TO GFCTV4-CNRO-ARQ-MOVTO.
           MOVE VE-CSEQ-MOVTO          OF VERCAREN
                                       TO GFCTV4-CSEQ-MOVTO.
           MOVE VE-CBCO-DSTNO-MOVTO    OF VERCAREN
                                       TO GFCTV4-CBCO-DSTNO-MOVTO.
           MOVE VE-CAG-DSTNO-MOVTO     OF VERCAREN
                                       TO GFCTV4-CAG-DSTNO-MOVTO.
           MOVE VE-CCTA-DSTNO-MOVTO    OF VERCAREN
                                       TO GFCTV4-CCTA-DSTNO-MOVTO.
           MOVE VE-CPAB-DSTNO-MOVTO    OF VERCAREN
                                       TO GFCTV4-CPAB-DSTNO-MOVTO.
           MOVE VE-CSERVC-TARIF        OF VERCAREN
                                       TO GFCTV4-CSERVC-TARIF.
           MOVE VE-DOCOR-EVNTO         OF VERCAREN
                                       TO GFCTV4-DOCOR-EVNTO.
           MOVE VE-HPREST-SERVC-MOVTO  OF VERCAREN
                                       TO GFCTV4-HPREST-MOVTO.
           MOVE WRK-TIMESTAMP-AUX      TO GFCTV4-IDTFD-TIMESTAMP.

VINI  *    XISPLAY 'VLR GRAVADO FRANQ41.' GFCTV4-VCONCS-DESC.

           WRITE GFCTV4-GFCTB041.

           PERFORM 1130-TESTAR-FS-FRANQT41.

           ADD 1                       TO ACU-GRAVA-FRANQT41.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CALCULAR DATA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3700-CALCULAR-DATA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-AUX           TO WRK-DT-INFERIOR-POOL.

           CALL 'POOL1285'             USING WRK-AREA-POOL1285.

           IF  RETURN-CODE             NOT EQUAL ZEROS

VINI  *        XISPLAY 'ERRO POOL1285 '
VINI  *        XISPLAY 'OCOR  '  WRK-POOL1205-OC
VINI  *        XISPLAY 'RET CD'  RETURN-CODE
VINI  *        XISPLAY 'DT INF'  WRK-DT-INFERIOR-POOL
VINI  *        XISPLAY 'QO DIA'  WRK-QTDE-DIAS-1285
VINI  *        XISPLAY 'DT SUP'  WRK-DT-SUPERIOR-POOL
VINI  *        XISPLAY 'MSG   '  WRK-MENSAGEM

               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-AREA-POOL1285  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-DT-SUPERIOR-POOL    TO WRK-DATA-AUX.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR SEGCOBRA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3900-GRAVAR-SEGCOBRA            SECTION.
      *----------------------------------------------------------------*

280409     MOVE GFCTVA-VTARIF-LIQ-MOVTO TO
280409                                  VE-VTARIF-LIQ-MOVTO OF SEGCOBRA.
280409     WRITE REG-IGFCTVE OF SEGCOBRA.

           PERFORM 1140-TESTAR-FS-SEGCOBRA.

           ADD 1                       TO ACU-GRAVA-SEGCOBRA.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO.         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-TOTAIS.

           CLOSE VERCAREN
                 FRANQT92
                 FRANQT41
                 SEGCOBRA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITIR TOTAIS                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-VERCAREN     TO WRK-MASCARA.

           DISPLAY '******************** GFCT1809 ******************'.
           DISPLAY '*                                              *'.
           DISPLAY '*              PROGRAMA GFCT1809               *'.
           DISPLAY '*              -----------------               *'.
           DISPLAY '*                                              *'.
           DISPLAY '*  LIDOS NO ARQUIVO VERCAREN...: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.

           DISPLAY '*  REGISTROS NAO PROCESSADOS...: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-FRANQT92     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO FRANQT92: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-FRANQT41     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO FRANQT41: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-SEGCOBRA     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO SEGCOBRA: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.
           DISPLAY '******************** GFCT1809 ******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1809'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


