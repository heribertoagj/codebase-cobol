      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM8110.
       AUTHOR.     FABIO AUGUSTO FINK.
      *================================================================*
      *                    D T S   C O N S U L T I N G                 *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM8110                                    *
      *    PROGRAMADOR.:   FABIO AUGUSTO FINK       - DTS CONSULTING   *
      *    ANALISTA DTS:   FABIO AUGUSTO FINK       - DTS CONSULTING   *
      *    ANALISTA BCO:   DALTON YUZO HIDAKA       - BRADESCO / GP.70 *
      *    DATA........:   01/09/2010                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   REATIVAR OPERACAO.                          *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                        INCLUDE/BOOK      *
      *                    EMOVREAT                    I#DCOMQY        *
      *                    SOPERAC                     I#DCOMQ6        *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#DCOM00 - AREA DE COMUNICACAO COM MODULOS DE RESTART       *
      *    I#DCOM64 - AREA PARA ACESSO AO MODULO DCOM6820.             *
      *    I#DCOM66 - AREA PARA ACESSO AO MODULO DCOM6830.             *
      *    I#DCOM68 - AREA PARA ACESSO AO MODULO DCOM6840.             *
      *    I#DCOMB6 - AREA PARA ACESSO AOS MODULOS DCOM7080/7083.      *
      *    I#DCOMD4 - AREA PARA ACESSO AO MODULO DCOM7170.             *
      *    I#DCOMF0 - AREA PARA ACESSO AO MODULO DCOM7251.             *
      *    I#DCOMI8 - AREA PARA ACESSO AO MODULO DCOM7450.             *
      *    I#CLIB19 - AREA PARA RETORNO DO CLIB2000.                   *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100.      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    DCOM6500 - SELECT NA TABELA DCOMB000 - CTRL_ROTINA_DESC     *
      *    DCOM6502 - INSERT NA TABELA DCOMB000 - CTRL_ROTINA_DESC     *
      *    DCOM6503 - UPDATE NA TABELA DCOMB000 - CTRL_ROTINA_DESC     *
      *    DCOM6504 - DELETE NA TABELA DCOMB000 - CTRL_ROTINA_DESC     *
      *    DCOM6820 - SELECT NA TABELA DCOMB064 - TSPROD_MEIO_ENTRD.   *
      *    DCOM6830 - SELECT NA TABELA DCOMB066 - TSPROD_MEIO_PGTO.    *
      *    DCOM6840 - SELECT NA TABELA DCOMB068 - TSPROD_REGRA_OPER.   *
      *    DCOM7080 - SELECT NA TABELA DCOMB0B6 - THIST_OPER_DESC.     *
      *    DCOM7083 - UPDATE NA TABELA DCOMB0B6 - THIST_OPER_DESC.     *
      *    DCOM7170 - SELECT NA TABELA DCOMB0D4 - TPSSOA_DESC_COML.    *
      *    DCOM7251 - SELECT NA TABELA DCOMB0F0 - TSPROD_GARNT_CREDT.  *
      *    DCOM7450 - SELECT NA TABELA DCOMB0I8 - THIST_PSSOA_OPER.    *
      *    CLIB2000 - ACESSO AS BASES CLIE / MULTIBANCO.               *
      *    BRAD1050 - CONEXAO COM DB2.                                 *
      *    BRAD7100 - TRATAMENTO DE ERROS.                             *
      *    BRAD7600 - OBTER DATA E HORA DO SISTEMA                     *
      *================================================================*
DTS001*                    ALTERACAO  -  DTS CONSULTING                *
DTS001*----------------------------------------------------------------*
DTS001*    PROGRAMADORA:   LUCIANDRA SILVEIRA       - DTS CONSULTING   *
DTS001*    ANALISTA DTS:   LUCIANDRA SILVEIRA       - DTS CONSULTING   *
DTS001*    ANALISTA DDS:   EDGARD R. DE SOUZA       - BRADESCO / GP.70 *
DTS001*    DATA........:   02/01/2012                                  *
DTS001*----------------------------------------------------------------*
DTS001*    OBJETIVO....:   OBTER TIPO DE COMERCIALIZACAO RURAL DA OPE- *
DTS001*                    RACAO DE NPR NO SISTEMA RURC.               *
DTS001*----------------------------------------------------------------*
DTS001*    BOOK'S......:                                               *
DTS001*         I#DCOMC0 - AREA DE COMUNICACAO COMO MODULO DCOM7100.   *
DTS001*         I#RURC10 - AREA DE COMUNICACAO COMO MODULO RURC9070.   *
DTS001*----------------------------------------------------------------*
DTS001*    MODULOS.....:                                               *
DTS001*         DCOM7100 - OBTER TITULO DA OPERACAO DE NP-RURAL.       *
DTS001*         RURC9050 - OBTER PRODUTO RECOR.                        *
DTS001*         RURC9070 - OBTER A FINALIDADE.                         *
DTS001*================================================================*

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

           SELECT EMOVREAT ASSIGN      TO UT-S-EMOVREAT
                      FILE STATUS      IS WRK-FS-EMOVREAT.

           SELECT SOPERAC  ASSIGN      TO UT-S-SOPERAC
                      FILE STATUS      IS WRK-FS-SOPERAC.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO MOVIMENTO DE REATIVACAO                     *
      *            ORG. SEQUENCIAL     - LRECL = 1630                  *
      *----------------------------------------------------------------*

       FD  EMOVREAT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#DCOMQY'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO OPERACAO DESCONTO COMERCIAL                 *
      *            ORG. SEQUENCIAL     - LRECL = 550                   *
      *----------------------------------------------------------------*

       FD  SOPERAC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#DCOMQ6'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING DCOM8110 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-EMOVREAT          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SOPERAC           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-UPDTE-DCOMB0B6          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-COMMIT                  PIC  9(009) COMP-3  VALUE ZEROS.

DTS001 77  ACU-OPERAC-NPR              PIC  9(009) COMP-3  VALUE ZEROS.
DTS001 77  ACU-NPR-AGRIC               PIC  9(009) COMP-3  VALUE ZEROS.
DTS001 77  ACU-NPR-PECUA               PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTE DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-EMOVREAT             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SOPERAC              PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-JOBNAME                 PIC  X(008)         VALUE SPACES.
       77  WRK-VALORFAC                PIC  9(005)V99      COMP-3
                                                           VALUE ZEROS.

       01  WRK-TIMESTAMP.
           05  WRK-ANO                 PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-MES                 PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-DIA                 PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-HORA                PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MINUTO              PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-SEGUNDO             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MILESIMOS           PIC  9(006)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CHAVES ***'.
      *----------------------------------------------------------------*

       01  WRK-CHV-EMOVREAT-ANT        PIC  X(013)         VALUE SPACES.

       01  WRK-CHV-EMOVREAT.
           05  WRK-EMOV-DANO-OPER      PIC  9(004)         VALUE ZEROS.
           05  WRK-EMOV-NSEQ-OPER      PIC  9(009)         VALUE ZEROS.

       01  WRK-CHAVE-RESTART.
           05  WRK-CHAVE-RESTART-AUX.
               10  WRK-DANO-OPER-DESC  PIC  9(004)         VALUE ZEROS.
               10  WRK-NSEQ-OPER-DESC  PIC  9(009)         VALUE ZEROS.
           05  WRK-REST-UPDTE-B0B6     PIC  9(009)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           05  WRK-7600-TIMESTAMP.
               10  WRK-7600-ANO        PIC  9(004)         VALUE ZEROS.
               10  WRK-7600-MES        PIC  9(002)         VALUE ZEROS.
               10  WRK-7600-DIA        PIC  9(002)         VALUE ZEROS.
               10  WRK-7600-HORA       PIC  9(002)         VALUE ZEROS.
               10  WRK-7600-MINUTO     PIC  9(002)         VALUE ZEROS.
               10  WRK-7600-SEGUNDO    PIC  9(002)         VALUE ZEROS.
               10  WRK-7600-MILESIMOS  PIC  9(006)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGEM-ERRO.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(018)         VALUE
               ' - FILE STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

DTS001 01  WRK-MENSA-RURC9070.
DTS001     05  FILLER                  PIC  X(036)         VALUE
DTS001         '* ERRO NO ACESSO RURC9070 - CRET. = '.
DTS001     05  WRK-COD-RURC9070        PIC  9(002)         VALUE ZEROS.
DTS001     05  FILLER                  PIC  X(003)         VALUE
DTS001         ' - '.
DTS001     05  WRK-MSG-RURC9070        PIC  X(032)         VALUE SPACES.
DTS001     05  FILLER                  PIC  X(002)         VALUE
DTS001         ' *'.
DTS001
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO CLIB2000 ***'.
      *----------------------------------------------------------------*

       01  WRK-CLIB-ENTRADA.
           05  WRK-CLIB-TRANSACAO      PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-CLIB-VERSAO         PIC  X(006)         VALUE SPACES.
           05  WRK-CLIB-PRODUTO        PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-CLIB-DADOS-ENT.
               10  WRK-CLIB-BANCO      PIC  9(005) COMP-3  VALUE ZEROS.
               10  WRK-CLIB-AGENCIA    PIC  9(005) COMP-3  VALUE ZEROS.
               10  WRK-CLIB-CONTA      PIC  9(013) COMP-3  VALUE ZEROS.
               10  FILLER              PIC  X(095)         VALUE SPACES.

       01  WRK-CLIB-SAIDA.
           05  WRK-CLIB-RC             PIC  9(003) COMP-3  VALUE ZEROS.
           05  WRK-CLIB-MENSAGEM.
               10  WRK-CLIB-ERRO-AREA  PIC  X(200)         VALUE SPACES.
               10  WRK-CLIB-SQLCA      PIC  X(200)         VALUE SPACES.
               10  WRK-CLIB-FILLER     PIC  X(598)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VALIDACAO NO HEXA ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMHX'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA RETORNO DO MODULO CLIB2000 ***'.
      *----------------------------------------------------------------*

       COPY 'I#CLIB19'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO DCOM6820 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOM64'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO DCOM6830 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOM66'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO DCOM6840 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOM68'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO DCOM7170 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMD4'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO DCOM7251 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMF0'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AO MODULO DCOM7450 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMI8'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACESSO AOS MODULOS DCOM7080/7083 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMB6'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '** AREA PARA ACESSO AOS MODULOS DE RESTART **'.
      *----------------------------------------------------------------*

       COPY 'I#DCOM00'.

DTS001*----------------------------------------------------------------*
DTS001 01  FILLER                      PIC  X(050)         VALUE
DTS001     '*** AREA PARA ACESSO AO MODULO DCOM7100 ***'.
DTS001*----------------------------------------------------------------*
DTS001
DTS001 COPY 'I#DCOMC0'.
DTS001
DTS001*----------------------------------------------------------------*
DTS001 01  FILLER                      PIC  X(050)         VALUE
DTS001     '*** AREA PARA ACESSO AO MODULO RURC9070 ***'.
DTS001*----------------------------------------------------------------*
DTS001
DTS001 COPY 'I#RURC10'.
DTS001
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '** AREA DE INCLUDE DB2 **'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING DCOM8110 ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *     ROTINA PRINCIPAL DO PROCESSAMENTO                          *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 2500-VERIFICAR-RESTART.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-EMOVREAT     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           OPEN INPUT  EMOVREAT
                OUTPUT SOPERAC.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TESTES DE FILE-STATUS DOS ARQUIVOS                         *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EMOVREAT.

           PERFORM 1120-TESTAR-FS-SOPERAC.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TESTE DE FILE-STATUS DO ARQUIVO EMOVREAT                   *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EMOVREAT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EMOVREAT         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'EMOVREAT'         TO WRK-NOME-ARQ
               MOVE  WRK-FS-EMOVREAT   TO WRK-FILE-STATUS
               MOVE  WRK-MENSAGEM-ERRO TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TESTE DE FILE-STATUS DO ARQUIVO SOPERAC                    *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-SOPERAC          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SOPERAC          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SOPERAC '         TO WRK-NOME-ARQ
               MOVE  WRK-FS-SOPERAC    TO WRK-FILE-STATUS
               MOVE  WRK-MENSAGEM-ERRO TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICAR SE ARQUIVOS DE ENTRADA ESTAO VAZIOS              *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-EMOVREAT.

           IF  WRK-FS-EMOVREAT         EQUAL '10'
               DISPLAY '************* DCOM8110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*      ARQUIVO EMOVREAT VAZIO      *'
               DISPLAY '*                                  *'
               DISPLAY '*     PROCESSAMENTO  ENCERRADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM8110 *************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     LEITURA DO ARQUIVO EMOVREAT                                *
      *----------------------------------------------------------------*
       2100-LER-EMOVREAT               SECTION.
      *----------------------------------------------------------------*

           READ EMOVREAT.

           IF  WRK-FS-EMOVREAT          EQUAL '10'
               MOVE HIGH-VALUES         TO WRK-CHV-EMOVREAT
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA             TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-EMOVREAT.

           MOVE QY-C3-CCHAVE-ELMTO-DESC(1:13)
                                        TO WRK-CHV-EMOVREAT.

           ADD 1                        TO ACU-LIDOS-EMOVREAT.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  VERIFICA SE HA REGISTRO NA TABELA DE CONTROLE DE ROTINAS      *
      *----------------------------------------------------------------*
       2500-VERIFICAR-RESTART          SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0160'             USING WRK-JOBNAME
                                             WRK-VALORFAC.

           INITIALIZE 00-ARGUMENTOS-ENTRADA
                      00-RETORNO.

           MOVE 'DCOM8110'             TO 00-CPROG.
           MOVE WRK-JOBNAME            TO 00-CJOB.
           MOVE 'DCOM6500'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 00-ARGUMENTOS-ENTRADA
                                             00-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  00-COD-RETORNO          NOT EQUAL ZEROS AND 03
               DISPLAY '*********** DCOM8110 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ERRO NO MODULO DCOM6500   *'
               DISPLAY '*    COD. RET. = ' 00-COD-RETORNO
                                            '          *'
               DISPLAY '*                              *'
               DISPLAY '*********** DCOM8110 ***********'
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  00-COD-RETORNO          EQUAL 03
               PERFORM 2510-INCLUIR-DCOMB000
           ELSE
               MOVE 00RT-WCHAVE-RESTT-TEXT(1)(1:22)
                                        TO WRK-CHAVE-RESTART
               MOVE WRK-REST-UPDTE-B0B6 TO ACU-UPDTE-DCOMB0B6
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  INCLUIR REGISTRO NA TABELA DE CONTROLE DE ROTINAS             *
      *----------------------------------------------------------------*
       2510-INCLUIR-DCOMB000           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 00-ARGUMENTOS-ENTRADA
                      00-RETORNO.

           MOVE 'DCOM8110'             TO 00-CPROG.
           MOVE WRK-JOBNAME            TO 00-CJOB.
           MOVE LENGTH OF WRK-CHAVE-RESTART
                                       TO 00-WCHAVE-RESTT-LEN.
           MOVE WRK-CHAVE-RESTART      TO 00-WCHAVE-RESTT-TEXT.
           MOVE 'DCOM6502'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 00-ARGUMENTOS-ENTRADA
                                             00-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  00-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '*********** DCOM8110 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ERRO NO MODULO DCOM6502   *'
               DISPLAY '*    COD. RET. = ' 00-COD-RETORNO
                                            '          *'
               DISPLAY '*                              *'
               DISPLAY '*********** DCOM8110 ***********'
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
            END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO PRINCIPAL                                    *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-ACESSAR-DCOM7080.

           PERFORM 3200-GRAVAR-SOPERAC.

           IF  WRK-CHV-EMOVREAT        GREATER WRK-CHAVE-RESTART-AUX
               PERFORM 3300-ACESSAR-DCOM7083
               ADD 1                   TO ACU-COMMIT
           END-IF.

           IF  ACU-COMMIT              NOT LESS 100
               PERFORM 3400-ATUALIZAR-DCOMB000
               PERFORM 3500-EFETUAR-COMMIT
           END-IF.

           MOVE WRK-CHV-EMOVREAT       TO WRK-CHV-EMOVREAT-ANT.

           PERFORM 2100-LER-EMOVREAT   UNTIL
                   WRK-CHV-EMOVREAT    NOT EQUAL WRK-CHV-EMOVREAT-ANT.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ACESSAR HISTORICO DA OPERACAO                              *
      *----------------------------------------------------------------*
       3100-ACESSAR-DCOM7080           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE B6-ARGUMENTOS-ENTRADA
                      B6-RETORNO
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE WRK-EMOV-DANO-OPER     TO B6-DANO-OPER-DESC.
           MOVE WRK-EMOV-NSEQ-OPER     TO B6-NSEQ-OPER-DESC.
           MOVE 'PARCIAL-01'           TO B6-INSTRUCAO.
           MOVE 'DCOM7080'             TO WRK-MODULO.

           CALL WRK-MODULO             USING B6-ARGUMENTOS-ENTRADA
                                             B6-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  B6-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '************* DCOM8110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*          ERRO NO ACESSO          *'
               DISPLAY '*        AO MODULO DCOM7080        *'
               DISPLAY '*                                  *'
               DISPLAY '* CODIGO RETORNO: ' B6-COD-RETORNO
                                             '             *'
               DISPLAY '* DANO-OPER-DESC: ' B6-DANO-OPER-DESC
                                             '             *'
               DISPLAY '* NSEQ-OPER-DESC: ' B6-NSEQ-OPER-DESC
                                                  '        *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM8110 *************'
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVAR ARQUIVO DE OPERACAO                                 *
      *     * PREENCHER DADOS COMPLEMENTARES                           *
      *----------------------------------------------------------------*
       3200-GRAVAR-SOPERAC             SECTION.
      *----------------------------------------------------------------*

           PERFORM 3210-OBTER-TIMESTAMP.

           MOVE SPACES                     TO Q6-C5-REGISTRO.

           INITIALIZE Q6-C5-REGISTRO.

           MOVE B6RT-DANO-OPER-DESC(1)     TO Q6-C5-DANO-OPER-DESC.
           MOVE B6RT-NSEQ-OPER-DESC(1)     TO Q6-C5-NSEQ-OPER-DESC.
           MOVE B6RT-CSIT-DESC-COML(1)     TO Q6-C5-CSIT-DESC-COML.
           MOVE B6RT-HSIT-DESC-COML(1)     TO Q6-C5-HSIT-DESC-COML.
           MOVE B6RT-VLIBRC-DESC-COML(1)   TO Q6-C5-VLIBRC-DESC-COML.
           MOVE B6RT-VLIBRC-ANTCP-OPER(1)  TO Q6-C5-VLIBRC-ANTCP-OPER.
           MOVE B6RT-CBCO(1)               TO Q6-C5-CBCO.
           MOVE B6RT-CAG-BCRIA(1)          TO Q6-C5-CAG-BCRIA.
           MOVE B6RT-CCTA-BCRIA-CLI(1)     TO Q6-C5-CCTA-BCRIA-CLI.
           MOVE B6RT-CBCO-OPER-OPER(1)     TO Q6-C5-CBCO-OPER-OPER.
           MOVE B6RT-CAG-OPER-OPER(1)      TO Q6-C5-CAG-OPER-OPER.
           MOVE B6RT-CPRODT(1)             TO Q6-C5-CPRODT.
           MOVE B6RT-CSPROD-DESC-COML(1)   TO Q6-C5-CSPROD-DESC-COML.
           MOVE B6RT-CTPO-DESC-COML(1)     TO Q6-C5-CTPO-DESC-COML.
           MOVE B6RT-CCONTR-CONVE-DESC(1)  TO Q6-C5-CCONTR-CONVE-DESC.
           MOVE B6RT-CVRSAO-CONTR-CONVE(1) TO Q6-C5-CVRSAO-CONTR-CONVE.
           MOVE B6RT-CELMTO-DESC-COML(1)   TO Q6-C5-CELMTO-DESC-COML.
           MOVE B6RT-NSMULA-OPER-DESC(1)   TO Q6-C5-NSMULA-OPER-DESC.
           MOVE B6RT-CINDCD-DIGTC-AG(1)    TO Q6-C5-CINDCD-DIGTC-AG.
           MOVE B6RT-CINDCD-ISENC-OPER(1)  TO Q6-C5-CINDCD-ISENC-OPER.
           MOVE B6RT-CINDCD-AUTRZ-OPER(1)  TO Q6-C5-CINDCD-AUTRZ-OPER.
           MOVE B6RT-CINDCD-MODLD-INCL(1)  TO Q6-C5-CINDCD-MODLD-INCL.
           MOVE B6RT-CCTRO-CUSTO(1)        TO Q6-C5-CCTRO-CUSTO.
           MOVE B6RT-DANO-BASE(1)          TO Q6-C5-DANO-BASE.
           MOVE B6RT-CDEPDC(1)             TO Q6-C5-CDEPDC.
           MOVE B6RT-CSEQ-STUDO(1)         TO Q6-C5-CSEQ-STUDO.
           MOVE B6RT-CINDCD-APROV-CREDT(1) TO Q6-C5-CINDCD-APROV-CREDT.
           MOVE B6RT-VOPER-DESC-COML(1)    TO Q6-C5-VOPER-DESC-COML.
           MOVE B6RT-DINIC-OPER-DESC(1)    TO Q6-C5-DINIC-OPER-DESC.
           MOVE B6RT-DVCTO-FNAL-OPER(1)    TO Q6-C5-DVCTO-FNAL-OPER.
           MOVE B6RT-TOPER-DESC-COML(1)    TO Q6-C5-TOPER-DESC-COML.
           MOVE B6RT-TMED-PONDE-OPER(1)    TO Q6-C5-TMED-PONDE-OPER.
           MOVE B6RT-CMEIO-ENTRD-DESC(1)   TO Q6-C5-CMEIO-ENTRD-DESC.
           MOVE B6RT-QPCELA-OPER(1)        TO Q6-C5-QPCELA-OPER.
           MOVE B6RT-QTITLO-CHEQ-PCELA(1)  TO Q6-C5-QTITLO-CHEQ-PCELA.
           MOVE B6RT-NAUTRZ-TRNSM-ESCR(1)  TO Q6-C5-NAUTRZ-TRNSM-ESCR.
           MOVE B6RT-CINDCD-PGDOR-JURO(1)  TO Q6-C5-CINDCD-PGDOR-JURO.
           MOVE B6RT-CTPO-TX-JURO-DESC(1)  TO Q6-C5-CTPO-TX-JURO-DESC.
           MOVE B6RT-PMIN-TX-JURO-REGRA(1) TO Q6-C5-PMIN-TX-JURO-REGRA.
           MOVE B6RT-PMAX-TX-JURO-REGRA(1) TO Q6-C5-PMAX-TX-JURO-REGRA.
           MOVE B6RT-PALIQT-IOF-DESC(1)    TO Q6-C5-PALIQT-IOF-DESC.
           MOVE B6RT-PTX-JURO-MES(1)       TO Q6-C5-PTX-JURO-MES.
           MOVE B6RT-PTX-JURO-ANO(1)       TO Q6-C5-PTX-JURO-ANO.
           MOVE B6RT-PTX-CUSTO-MES(1)      TO Q6-C5-PTX-CUSTO-MES.
           MOVE B6RT-PTX-CUSTO-ANO(1)      TO Q6-C5-PTX-CUSTO-ANO.
           MOVE B6RT-VJURO-OPER-DESC(1)    TO Q6-C5-VJURO-OPER-DESC.
           MOVE B6RT-VJURO-CUSTO-OPER(1)   TO Q6-C5-VJURO-CUSTO-OPER.
           MOVE B6RT-VTARIF-REG-TITLO(1)   TO Q6-C5-VTARIF-REG-TITLO.
           MOVE B6RT-VTAC-OPER-DESC(1)     TO Q6-C5-VTAC-OPER-DESC.
           MOVE B6RT-VPSENT-OPER-DESC(1)   TO Q6-C5-VPSENT-OPER-DESC.
           MOVE B6RT-VIOF-OPER-DESC(1)     TO Q6-C5-VIOF-OPER-DESC.
           MOVE B6RT-VLIQ-OPER-DESC(1)     TO Q6-C5-VLIQ-OPER-DESC.
           MOVE B6RT-CINDCD-VALDC-ROTRO(1) TO Q6-C5-CINDCD-VALDC-ROTRO.
           MOVE B6RT-DVALDC-ROTRO-ELMTO(1) TO Q6-C5-DVALDC-ROTRO-ELMTO.
           MOVE B6RT-CFUNC-VALDC-ROTRO(1)  TO Q6-C5-CFUNC-VALDC-ROTRO.
           MOVE B6RT-DULT-ADTTO-OPER(1)    TO Q6-C5-DULT-ADTTO-OPER.
           MOVE SPACES                     TO Q6-C5-DULT-ADTTO-NULL.
           MOVE B6RT-CINDCD-MOTVO-ADTTO(1) TO Q6-C5-CINDCD-MOTVO-ADTTO.
           MOVE SPACES                     TO Q6-C5-CINDCD-MOTVO-NULL.
           MOVE WRK-TIMESTAMP              TO Q6-C5-HULT-ATULZ.
           MOVE QY-C3-CFUNC-BDSCO          TO Q6-C5-CFUNC-BDSCO.
           MOVE QY-C3-CTERM                TO Q6-C5-CTERM.
           MOVE B6RT-CCONTR-CORP-OPER(1)   TO Q6-C5-CCONTR-CORP-OPER.
           MOVE B6RT-DINCL-REG(1)          TO Q6-C5-DINCL-REG.
           MOVE B6RT-CCART-LIM-CREDT(1)    TO Q6-C5-CCART-LIM-CREDT.
           MOVE WRK-CHV-EMOVREAT           TO Q6-C5-CCHAVE-ELMTO-DESC.
           MOVE B6RT-CINDCD-ALIQT-REDZD(1) TO Q6-C5-CINDCD-ALIQT-REDZD.
           MOVE B6RT-PTX-JURO-DIA(1)       TO Q6-C5-PTX-JURO-DIA.
           MOVE B6RT-CINDCD-FREQ-TX(1)     TO Q6-C5-CINDCD-FREQ-TX.

           PERFORM 3215-ACESSAR-DCOM6820.

           PERFORM 3220-ACESSAR-DCOM7450.

           PERFORM 3225-ACESSAR-DCOM7170.

           PERFORM 3230-ACESSAR-DCOM6830-1.

           PERFORM 3235-ACESSAR-DCOM6830-2.

           PERFORM 3240-ACESSAR-CLIB2000.

           PERFORM 3245-ACESSAR-DCOM6840.

           PERFORM 3250-ACESSAR-DCOM7251.

DTS001     IF  B6RT-CTPO-DESC-COML(1)      EQUAL 007 OR 008
DTS001         PERFORM 3255-OBTER-TITULO
DTS001         PERFORM 3256-OBTER-COMERCIALIZACAO
DTS001     END-IF.
DTS001
           MOVE WRK-GRAVACAO               TO WRK-OPERACAO.

           WRITE Q6-C5-REGISTRO.

           PERFORM 1120-TESTAR-FS-SOPERAC.

           ADD 1                           TO ACU-GRAVA-SOPERAC.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTER DATA E HORA DO SISTEMA                               *
      *----------------------------------------------------------------*
       3210-OBTER-TIMESTAMP            SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

           MOVE WRK-7600-ANO           TO WRK-ANO.
           MOVE WRK-7600-MES           TO WRK-MES.
           MOVE WRK-7600-DIA           TO WRK-DIA.
           MOVE WRK-7600-HORA          TO WRK-HORA.
           MOVE WRK-7600-MINUTO        TO WRK-MINUTO.
           MOVE WRK-7600-SEGUNDO       TO WRK-SEGUNDO.
           MOVE WRK-7600-MILESIMOS     TO WRK-MILESIMOS.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTER CARTEIRAS                                            *
      *----------------------------------------------------------------*
       3215-ACESSAR-DCOM6820           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 64-ARGUMENTOS-ENTRADA
                      64-RETORNO
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE Q6-C5-CPRODT           TO 64-CPRODT.
           MOVE Q6-C5-CSPROD-DESC-COML TO 64-CSPROD-DESC-COML.
           MOVE Q6-C5-CMEIO-ENTRD-DESC TO 64-CMEIO-ENTRD-DESC.
           MOVE Q6-C5-DINIC-OPER-DESC(1:2)
                                       TO 64-HINIC-MEIO-ENTRD(9:2).
           MOVE Q6-C5-DINIC-OPER-DESC(4:2)
                                       TO 64-HINIC-MEIO-ENTRD(6:2).
           MOVE Q6-C5-DINIC-OPER-DESC(7:4)
                                       TO 64-HINIC-MEIO-ENTRD(1:4).
           MOVE 'PARCIAL-07'           TO 64-INSTRUCAO.
           MOVE 'DCOM6820'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 64-ARGUMENTOS-ENTRADA
                                             64-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  64-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '************* DCOM8110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*          ERRO NO ACESSO          *'
               DISPLAY '*        AO MODULO DCOM6820        *'
               DISPLAY '*                                  *'
               DISPLAY '* CODIGO RETORNO: ' 64-COD-RETORNO
                                             '             *'
               DISPLAY '* DANO-OPER-DESC: ' Q6-C5-DANO-OPER-DESC
                                             '             *'
               DISPLAY '* NSEQ-OPER-DESC: ' Q6-C5-NSEQ-OPER-DESC
                                                  '        *'
               DISPLAY '* CPRODT        : ' Q6-C5-CPRODT
                                            '              *'
               DISPLAY '* CSPROD-DESC   : ' Q6-C5-CSPROD-DESC-COML
                                            '              *'
               DISPLAY '* CMEIO-ENTRD   : ' Q6-C5-CMEIO-ENTRD-DESC
                                            '              *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM8110 *************'
               MOVE WRK-MODULO           TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE 64RT-CCART-NORML-DESC(1) TO Q6-C5-CCART-NORML-DESC.
           MOVE 64RT-CCART-VENCD-DESC(1) TO Q6-C5-CCART-VENCD-DESC.
           MOVE 64RT-CCART-DESC-PRODT(1) TO Q6-C5-CCART-DESC-PRODT.

      *----------------------------------------------------------------*
       3215-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTER DADOS DO CEDENTE                                     *
      *----------------------------------------------------------------*
       3220-ACESSAR-DCOM7450           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE I8-ARGUMENTOS-ENTRADA
                      I8-RETORNO
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE Q6-C5-DANO-OPER-DESC   TO I8-DANO-OPER-DESC.
           MOVE Q6-C5-NSEQ-OPER-DESC   TO I8-NSEQ-OPER-DESC.
           MOVE ZEROS                  TO I8-NPCELA-DESC-COML.
           MOVE 'CP'                   TO I8-CINDCD-PSSOA-DESC.
           MOVE 'PARCIAL-02'           TO I8-INSTRUCAO.
           MOVE 'DCOM7450'             TO WRK-MODULO.

           CALL WRK-MODULO             USING I8-ARGUMENTOS-ENTRADA
                                             I8-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  I8-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '************* DCOM8110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*          ERRO NO ACESSO          *'
               DISPLAY '*        AO MODULO DCOM7450        *'
               DISPLAY '*                                  *'
               DISPLAY '* CODIGO RETORNO: ' I8-COD-RETORNO
                                             '             *'
               DISPLAY '* DANO-OPER-DESC: ' Q6-C5-DANO-OPER-DESC
                                             '             *'
               DISPLAY '* NSEQ-OPER-DESC: ' Q6-C5-NSEQ-OPER-DESC
                                                  '        *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM8110 *************'
               MOVE WRK-MODULO           TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE I8RT-CCNPJ-CPF(1)        TO Q6-C5-CCNPJ-CPF.
           MOVE I8RT-CFLIAL-CNPJ(1)      TO Q6-C5-CFLIAL-CNPJ.
           MOVE I8RT-CCTRL-CNPJ-CPF(1)   TO Q6-C5-CCTRL-CNPJ-CPF.
           MOVE I8RT-IPSSOA-DESC-COML(1) TO Q6-C5-CEDENTE.

           IF  I8RT-CFLIAL-CNPJ(1)       EQUAL ZEROS
               MOVE 'F'                  TO Q6-C5-TIPO-PSSOA
           ELSE
               MOVE 'J'                  TO Q6-C5-TIPO-PSSOA
           END-IF.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTER NUMERO DA PESSOA                                     *
      *----------------------------------------------------------------*
       3225-ACESSAR-DCOM7170           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE D4-ARGUMENTOS-ENTRADA
                      D4-RETORNO
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE I8RT-CCNPJ-CPF(1)      TO D4-CCNPJ-CPF.
           MOVE I8RT-CFLIAL-CNPJ(1)    TO D4-CFLIAL-CNPJ.
           MOVE I8RT-CCTRL-CNPJ-CPF(1) TO D4-CCTRL-CNPJ-CPF.
           MOVE 'PARCIAL-01'           TO D4-INSTRUCAO.
           MOVE 'DCOM7170'             TO WRK-MODULO.

           CALL WRK-MODULO             USING D4-ARGUMENTOS-ENTRADA
                                             D4-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  D4-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '************* DCOM8110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*          ERRO NO ACESSO          *'
               DISPLAY '*        AO MODULO DCOM7170        *'
               DISPLAY '*                                  *'
               DISPLAY '* CODIGO RETORNO: ' D4-COD-RETORNO
                                             '             *'
               DISPLAY '* DANO-OPER-DESC: ' Q6-C5-DANO-OPER-DESC
                                             '             *'
               DISPLAY '* NSEQ-OPER-DESC: ' Q6-C5-NSEQ-OPER-DESC
                                                  '        *'
               DISPLAY '* CCNPJ-CPF     : ' I8RT-CCNPJ-CPF(1)
                                                  '        *'
               DISPLAY '* CFLIAL-CNPJ   : ' I8RT-CFLIAL-CNPJ(1)
                                              '            *'
               DISPLAY '* CCTRL-CNPJ-CPF: ' I8RT-CCTRL-CNPJ-CPF(1)
                                           '               *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM8110 *************'
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE D4RT-NPSSOA-DESC-COML(1) TO Q6-C5-NPSSOA-DESC-COML.

      *----------------------------------------------------------------*
       3225-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTER MEIO DE PAGAMENTO A VENCER                           *
      *----------------------------------------------------------------*
       3230-ACESSAR-DCOM6830-1         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 66-ARGUMENTOS-ENTRADA
                      66-RETORNO
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE Q6-C5-CPRODT           TO 66-CPRODT.
           MOVE Q6-C5-CSPROD-DESC-COML TO 66-CSPROD-DESC-COML.
           MOVE 'T'                    TO 66-CINDCD-VCTO-PCELA.
           MOVE 'PARCIAL-10'           TO 66-INSTRUCAO.
           MOVE 'DCOM6830'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 66-ARGUMENTOS-ENTRADA
                                             66-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  66-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '************* DCOM8110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*          ERRO NO ACESSO          *'
               DISPLAY '*        AO MODULO DCOM6830 - 1    *'
               DISPLAY '*                                  *'
               DISPLAY '* CODIGO RETORNO: ' 66-COD-RETORNO
                                             '             *'
               DISPLAY '* DANO-OPER-DESC: ' Q6-C5-DANO-OPER-DESC
                                             '             *'
               DISPLAY '* NSEQ-OPER-DESC: ' Q6-C5-NSEQ-OPER-DESC
                                                  '        *'
               DISPLAY '* CPRODT        : ' Q6-C5-CPRODT
                                            '              *'
               DISPLAY '* CSPROD-DESC   : ' Q6-C5-CSPROD-DESC-COML
                                            '              *'
               DISPLAY '* CINDCD-VCTO   : ' 66-CINDCD-VCTO-PCELA
                                          '                *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM8110 *************'
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE 66RT-CMEIO-PGTO-DESC(1)  TO Q6-C5-CMEIO-PGTO-DESC.

      *----------------------------------------------------------------*
       3230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTER MEIO DE PAGAMENTO VENCIDO                            *
      *----------------------------------------------------------------*
       3235-ACESSAR-DCOM6830-2         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 66-ARGUMENTOS-ENTRADA
                      66-RETORNO
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE Q6-C5-CPRODT           TO 66-CPRODT.
           MOVE Q6-C5-CSPROD-DESC-COML TO 66-CSPROD-DESC-COML.
           MOVE 'P'                    TO 66-CINDCD-VCTO-PCELA.
           MOVE 'PARCIAL-10'           TO 66-INSTRUCAO.
           MOVE 'DCOM6830'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 66-ARGUMENTOS-ENTRADA
                                             66-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  66-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '************* DCOM8110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*          ERRO NO ACESSO          *'
               DISPLAY '*        AO MODULO DCOM6830 - 2    *'
               DISPLAY '*                                  *'
               DISPLAY '* CODIGO RETORNO: ' 66-COD-RETORNO
                                             '             *'
               DISPLAY '* DANO-OPER-DESC: ' Q6-C5-DANO-OPER-DESC
                                             '             *'
               DISPLAY '* NSEQ-OPER-DESC: ' Q6-C5-NSEQ-OPER-DESC
                                                  '        *'
               DISPLAY '* CPRODT        : ' Q6-C5-CPRODT
                                            '              *'
               DISPLAY '* CSPROD-DESC   : ' Q6-C5-CSPROD-DESC-COML
                                            '              *'
               DISPLAY '* CINDCD-VCTO   : ' 66-CINDCD-VCTO-PCELA
                                          '                *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM8110 *************'
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE 66RT-CMEIO-PGTO-DESC(1)  TO Q6-C5-CMEIO-PGTO-VENCD.
           MOVE 66RT-CINDCD-RESP-PGTO(1) TO Q6-C5-CINDCD-RESP-PGTO.
           MOVE 66RT-TDEB-PCELA-VENCD(1) TO Q6-C5-TDEB-PCELA-VENCD.
           MOVE 66RT-CINDCD-PRZ-PCELA(1) TO Q6-C5-CINDCD-PRZ-PCELA.

      *----------------------------------------------------------------*
       3235-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICAR CLIENTE CORRENTISTA                              *
      *----------------------------------------------------------------*
       3240-ACESSAR-CLIB2000           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-CLIB-ENTRADA
                      WRK-CLIB-SAIDA.

           MOVE 3                      TO WRK-CLIB-TRANSACAO
           MOVE 'VRS000'               TO WRK-CLIB-VERSAO.
           MOVE 1                      TO WRK-CLIB-PRODUTO.
           MOVE Q6-C5-CBCO             TO WRK-CLIB-BANCO.
           MOVE Q6-C5-CAG-BCRIA        TO WRK-CLIB-AGENCIA.
           MOVE Q6-C5-CCTA-BCRIA-CLI   TO WRK-CLIB-CONTA.
           MOVE 'CLIB2000'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-CLIB-ENTRADA
                                             WRK-CLIB-SAIDA.

           IF  WRK-CLIB-RC             NOT EQUAL ZEROS
               DISPLAY '************* DCOM8110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*          ERRO NO ACESSO          *'
               DISPLAY '*        AO MODULO CLIB2000        *'
               DISPLAY '*                                  *'
               DISPLAY '* CODIGO RETORNO: ' WRK-CLIB-RC
                                            '              *'
               DISPLAY '* DANO-OPER-DESC: ' Q6-C5-DANO-OPER-DESC
                                             '             *'
               DISPLAY '* NSEQ-OPER-DESC: ' Q6-C5-NSEQ-OPER-DESC
                                                  '        *'
               DISPLAY '* CAG-BCRIA     : ' Q6-C5-CAG-BCRIA
                                              '            *'
               DISPLAY '* CCTA-BCRIA-CLI: ' Q6-C5-CCTA-BCRIA-CLI
                                                      '    *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM8110 *************'
               IF  WRK-CLIB-RC             EQUAL 02
                   MOVE 'APL'              TO ERR-TIPO-ACESSO
                   MOVE WRK-CLIB-MENSAGEM(1:75)
                                           TO ERR-TEXTO
               ELSE
                   MOVE WRK-CLIB-ERRO-AREA(1:107)
                                           TO ERRO-AREA
                   MOVE WRK-CLIB-SQLCA(1:136)
                                           TO WRK-SQLCA
               END-IF
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-CLIB-MENSAGEM      TO WRK-CLIBB007.

           IF  WRK-CGRP-CTBIL-ATUAL    EQUAL 63 AND
               WRK-CSGRP-CTBIL         EQUAL 52
               MOVE 'N'                TO Q6-C5-CORRENTISTA
           ELSE
               MOVE 'S'                TO Q6-C5-CORRENTISTA
           END-IF.

      *----------------------------------------------------------------*
       3240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTER TIPO DE CALCULO                                      *
      *----------------------------------------------------------------*
       3245-ACESSAR-DCOM6840           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 68-ARGUMENTOS-ENTRADA
                      68-RETORNO
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE Q6-C5-CPRODT           TO 68-CPRODT.
           MOVE Q6-C5-CSPROD-DESC-COML TO 68-CSPROD-DESC-COML.
           MOVE '0000-00-00-00.00.01.000000'
                                       TO 68-HINIC-REGRA-OPER
                                          68-HFIM-REGRA-OPER.
           MOVE Q6-C5-DINIC-OPER-DESC(1:2)
                                       TO 68-HINIC-REGRA-OPER(9:2)
                                          68-HFIM-REGRA-OPER(9:2).
           MOVE Q6-C5-DINIC-OPER-DESC(4:2)
                                       TO 68-HINIC-REGRA-OPER(6:2)
                                          68-HFIM-REGRA-OPER(6:2).
           MOVE Q6-C5-DINIC-OPER-DESC(7:4)
                                       TO 68-HINIC-REGRA-OPER(1:4)
                                          68-HFIM-REGRA-OPER(1:4).
           MOVE 'PARCIAL-05'           TO 68-INSTRUCAO.
           MOVE 'DCOM6840'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 68-ARGUMENTOS-ENTRADA
                                             68-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  66-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '************* DCOM8110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*          ERRO NO ACESSO          *'
               DISPLAY '*        AO MODULO DCOM6830 - 2    *'
               DISPLAY '*                                  *'
               DISPLAY '* CODIGO RETORNO: ' 66-COD-RETORNO
                                             '             *'
               DISPLAY '* DANO-OPER-DESC: ' Q6-C5-DANO-OPER-DESC
                                             '             *'
               DISPLAY '* NSEQ-OPER-DESC: ' Q6-C5-NSEQ-OPER-DESC
                                                  '        *'
               DISPLAY '* CPRODT        : ' Q6-C5-CPRODT
                                            '              *'
               DISPLAY '* CSPROD-DESC   : ' Q6-C5-CSPROD-DESC-COML
                                            '              *'
               DISPLAY '* CINDCD-VCTO   : ' 66-CINDCD-VCTO-PCELA
                                          '                *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM8110 *************'
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  68RT-CINDCD-CALC-JURO(1) EQUAL 'L'
               MOVE 1                   TO Q6-C5-CINDCD-CALC-JURO
           ELSE
               MOVE 2                   TO Q6-C5-CINDCD-CALC-JURO
           END-IF.

      *----------------------------------------------------------------*
       3245-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTER GARANTIA DE CREDITO                                  *
      *----------------------------------------------------------------*
       3250-ACESSAR-DCOM7251           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE F0-ARGUMENTOS-ENTRADA
                      F0-RETORNO
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE Q6-C5-CPRODT           TO F0-CPRODT.
           MOVE Q6-C5-CSPROD-DESC-COML TO F0-CSPROD-DESC-COML.
           MOVE Q6-C5-CMEIO-ENTRD-DESC TO F0-CMEIO-ENTRD-DESC.
           MOVE '0000-00-00-00.00.01.000000'
                                       TO F0-HINIC-VGCIA-DESC
                                          F0-HFIM-VGCIA-DESC.
           MOVE Q6-C5-DINIC-OPER-DESC(1:2)
                                       TO F0-HINIC-VGCIA-DESC(9:2)
                                          F0-HFIM-VGCIA-DESC(9:2).
           MOVE Q6-C5-DINIC-OPER-DESC(4:2)
                                       TO F0-HINIC-VGCIA-DESC(6:2)
                                          F0-HFIM-VGCIA-DESC(6:2).
           MOVE Q6-C5-DINIC-OPER-DESC(7:4)
                                       TO F0-HINIC-VGCIA-DESC(1:4)
                                          F0-HFIM-VGCIA-DESC(1:4).
           MOVE 'PARCIAL-03'           TO F0-INSTRUCAO.
           MOVE 'DCOM7251'             TO WRK-MODULO.

           CALL WRK-MODULO             USING F0-ARGUMENTOS-ENTRADA
                                             F0-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  F0-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '************* DCOM8110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*          ERRO NO ACESSO          *'
               DISPLAY '*        AO MODULO DCOM7251        *'
               DISPLAY '*                                  *'
               DISPLAY '* CODIGO RETORNO: ' 66-COD-RETORNO
                                             '             *'
               DISPLAY '* DANO-OPER-DESC: ' Q6-C5-DANO-OPER-DESC
                                             '             *'
               DISPLAY '* NSEQ-OPER-DESC: ' Q6-C5-NSEQ-OPER-DESC
                                                  '        *'
               DISPLAY '* CPRODT        : ' Q6-C5-CPRODT
                                            '              *'
               DISPLAY '* CSPROD-DESC   : ' Q6-C5-CSPROD-DESC-COML
                                            '              *'
               DISPLAY '* CMEIO-ENTRD   : ' Q6-C5-CMEIO-ENTRD-DESC
                                            '              *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM8110 *************'
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE F0RT-CTPO-GARNT(1)     TO Q6-C5-CTPO-GARNT.

      *----------------------------------------------------------------*
       3250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

DTS001*----------------------------------------------------------------*
DTS001*     OBTER TITULO DE NP-RURAL DA OPERACAO                       *
DTS001*----------------------------------------------------------------*
DTS001 3255-OBTER-TITULO               SECTION.
DTS001*----------------------------------------------------------------*
DTS001
DTS001     INITIALIZE C0-ARGUMENTOS-ENTRADA
DTS001                C0-RETORNO.
DTS001
DTS001     MOVE B6RT-DANO-OPER-DESC(1) TO C0-DANO-OPER-DESC.
DTS001     MOVE B6RT-NSEQ-OPER-DESC(1) TO C0-NSEQ-OPER-DESC.
DTS001     MOVE 001                    TO C0-NTITLO-DESC-COML.
DTS001     MOVE B6RT-HULT-ATULZ    (1) TO C0-HULT-ATULZ.
DTS001     MOVE 'TOTAL'                TO C0-INSTRUCAO.
DTS001     MOVE 'DCOM7100'             TO WRK-MODULO.
DTS001
DTS001     CALL WRK-MODULO             USING C0-ARGUMENTOS-ENTRADA
DTS001                                       C0-RETORNO
DTS001                                       ERRO-AREA
DTS001                                       WRK-SQLCA.
DTS001
DTS001     IF  C0-COD-RETORNO          NOT EQUAL ZEROS
DTS001         DISPLAY '************* DCOM8110 *************'
DTS001         DISPLAY '*                                  *'
DTS001         DISPLAY '*          ERRO NO ACESSO          *'
DTS001         DISPLAY '*        AO MODULO DCOM7100        *'
DTS001         DISPLAY '*                                  *'
DTS001         DISPLAY '* CODIGO RETORNO..: ' C0-COD-RETORNO
DTS001                                         '           *'
DTS001         DISPLAY '* DANO-OPER-DESC..: ' C0-DANO-OPER-DESC
DTS001                                         '           *'
DTS001         DISPLAY '* NSEQ-OPER-DESC..: ' C0-NSEQ-OPER-DESC
DTS001                                              '      *'
DTS001         DISPLAY '* NTITLO-DESC-COML: ' C0-NTITLO-DESC-COML
DTS001                                          '          *'
DTS001         DISPLAY '* HULT-ATULZ......: ' C0-HULT-ATULZ
DTS001
DTS001         DISPLAY '*      PROCESSAMENTO CANCELADO     *'
DTS001         DISPLAY '*                                  *'
DTS001         DISPLAY '************* DCOM8110 *************'
DTS001         MOVE WRK-MODULO         TO ERR-MODULO
DTS001         PERFORM 9999-PROCESSAR-ROTINA-ERRO
DTS001     END-IF.
DTS001
DTS001     ADD 1                       TO ACU-OPERAC-NPR.
DTS001
DTS001*----------------------------------------------------------------*
DTS001 3255-99-FIM.                    EXIT.
DTS001*----------------------------------------------------------------*
DTS001
DTS001*----------------------------------------------------------------*
DTS001*     OBTER TIPO COMERCIALIZACAO PARA OPERACOES DE NP-RURAL      *
DTS001*----------------------------------------------------------------*
DTS001 3256-OBTER-COMERCIALIZACAO      SECTION.
DTS001*----------------------------------------------------------------*
DTS001
DTS001     INITIALIZE AREA-ENT-RURC10
DTS001                AREA-SAI-RURC10
DTS001                ERRO-AREA
DTS001                WRK-SQLCA.
DTS001
DTS001     MOVE 70                     TO CFNALD-RURAL-RURC10.
DTS001     MOVE C0RT-CPRODT-RURAL(1)   TO HX-NUM-03-CS.
DTS001     MOVE HX-NUM-03-SS           TO CPRODT-RURAL-RURC10.
DTS001     MOVE 'N'                    TO RESTART-RURC10.
DTS001     MOVE 'U'                    TO TIPO-RETORNO-RURC10.
DTS001     MOVE 'RURC9070'             TO WRK-MODULO.
DTS001
DTS001     CALL WRK-MODULO             USING AREA-ENT-RURC10
DTS001                                       AREA-SAI-RURC10
DTS001                                       ERRO-AREA
DTS001                                       WRK-SQLCA.
DTS001
DTS001     IF  COD-RETORNO-RURC10      NOT EQUAL ZEROS AND 10
DTS001         EVALUATE COD-RETORNO-RURC10
DTS001             WHEN 01
DTS001                  MOVE 'HA MAIS DE 1 OCORR P/ O CRITERIO'
DTS001                                 TO WRK-MSG-RURC9070
DTS001             WHEN 20
DTS001                  MOVE 'DADOS DE ENTRADA INCONSISTENTE'
DTS001                                 TO WRK-MSG-RURC9070
DTS001             WHEN 99
DTS001                  MOVE 'ERROS DE DB2'
DTS001                                 TO WRK-MSG-RURC9070
DTS001         END-EVALUATE
DTS001         DISPLAY '******************* DCOM8110 ******************'
DTS001         DISPLAY '*                                             *'
DTS001         DISPLAY '*          ERRO NO ACESSO AO RURC9070         *'
DTS001         DISPLAY '*                                             *'
DTS001         DISPLAY '*  COD-RETORNO....: ' COD-RETORNO-RURC10
DTS001                                       '                        *'
DTS001         DISPLAY '*  MENSAGEM.......: ' WRK-MSG-RURC9070
DTS001
DTS001         DISPLAY '*  DANO-OPER-DESC.: ' C0-DANO-OPER-DESC
DTS001                                         '                      *'
DTS001         DISPLAY '*  NSEQ-OPER-DESC.: ' C0-NSEQ-OPER-DESC
DTS001                                              '                 *'
DTS001         DISPLAY '*  CFNALD-RURAL...: ' CFNALD-RURAL-RURC10
DTS001                                        '                       *'
DTS001         DISPLAY '*  CPRODT-RURAL...: ' CPRODT-RURAL-RURC10
DTS001                                        '                       *'
DTS001         DISPLAY '*            PROCESSAMENTO CANCELADO          *'
DTS001         DISPLAY '*                                             *'
DTS001         DISPLAY '******************* DCOM8110 ******************'
DTS001         MOVE 'APL'              TO ERR-TIPO-ACESSO
DTS001         MOVE WRK-MODULO         TO ERR-MODULO
DTS001         MOVE COD-RETORNO-RURC10 TO WRK-COD-RURC9070
DTS001         MOVE WRK-MENSA-RURC9070 TO ERR-TEXTO
DTS001         PERFORM 9999-PROCESSAR-ROTINA-ERRO
DTS001     END-IF.
DTS001
DTS001     IF  COD-RETORNO-RURC10      EQUAL ZEROS
DTS001         MOVE 'G'                TO Q6-C5-TPO-COMCZ-RURAL
DTS001         ADD 1                   TO ACU-NPR-AGRIC
DTS001         GO TO 3256-99-FIM
DTS001     END-IF.
DTS001
DTS001     INITIALIZE AREA-ENT-RURC10
DTS001                AREA-SAI-RURC10
DTS001                ERRO-AREA
DTS001                WRK-SQLCA.
DTS001
DTS001     MOVE 71                     TO CFNALD-RURAL-RURC10.
DTS001     MOVE C0RT-CPRODT-RURAL(1)   TO HX-NUM-03-CS.
DTS001     MOVE HX-NUM-03-SS           TO CPRODT-RURAL-RURC10.
DTS001     MOVE 'N'                    TO RESTART-RURC10.
DTS001     MOVE 'U'                    TO TIPO-RETORNO-RURC10.
DTS001     MOVE 'RURC9070'             TO WRK-MODULO.
DTS001
DTS001     CALL WRK-MODULO             USING AREA-ENT-RURC10
DTS001                                       AREA-SAI-RURC10
DTS001                                       ERRO-AREA
DTS001                                       WRK-SQLCA.
DTS001
DTS001     IF  COD-RETORNO-RURC10      NOT EQUAL ZEROS
DTS001         EVALUATE COD-RETORNO-RURC10
DTS001             WHEN 01
DTS001                  MOVE 'HA MAIS OCORRENCIA P/ O CRITERIO'
DTS001                                 TO WRK-MSG-RURC9070
DTS001             WHEN 10
DTS001                  MOVE 'PRODUTO RURAL INEXISTENTE'
DTS001                                 TO WRK-MSG-RURC9070
DTS001             WHEN 20
DTS001                  MOVE 'DADOS DE ENTRADA INCONSISTENTE'
DTS001                                 TO WRK-MSG-RURC9070
DTS001             WHEN 99
DTS001                  MOVE 'ERROS DE DB2'
DTS001                                 TO WRK-MSG-RURC9070
DTS001         END-EVALUATE
DTS001         DISPLAY '******************* DCOM8110 ******************'
DTS001         DISPLAY '*                                             *'
DTS001         DISPLAY '*          ERRO NO ACESSO AO RURC9070         *'
DTS001         DISPLAY '*                                             *'
DTS001         DISPLAY '*  COD-RETORNO....: ' COD-RETORNO-RURC10
DTS001                                       '                        *'
DTS001         DISPLAY '*  MENSAGEM.......: ' WRK-MSG-RURC9070
DTS001
DTS001         DISPLAY '*  DANO-OPER-DESC.: ' C0-DANO-OPER-DESC
DTS001                                         '                      *'
DTS001         DISPLAY '*  NSEQ-OPER-DESC.: ' C0-NSEQ-OPER-DESC
DTS001                                              '                 *'
DTS001         DISPLAY '*  CFNALD-RURAL...: ' CFNALD-RURAL-RURC10
DTS001                                        '                       *'
DTS001         DISPLAY '*  CPRODT-RURAL...: ' CPRODT-RURAL-RURC10
DTS001                                        '                       *'
DTS001         DISPLAY '*            PROCESSAMENTO CANCELADO          *'
DTS001         DISPLAY '*                                             *'
DTS001         DISPLAY '******************* DCOM8110 ******************'
DTS001         MOVE 'APL'              TO ERR-TIPO-ACESSO
DTS001         MOVE WRK-MODULO         TO ERR-MODULO
DTS001         MOVE COD-RETORNO-RURC10 TO WRK-COD-RURC9070
DTS001         MOVE WRK-MENSA-RURC9070 TO ERR-TEXTO
DTS001         PERFORM 9999-PROCESSAR-ROTINA-ERRO
DTS001     ELSE
DTS001         MOVE 'P'                TO Q6-C5-TPO-COMCZ-RURAL
DTS001         ADD 1                   TO ACU-NPR-PECUA
DTS001     END-IF.
DTS001
DTS001*----------------------------------------------------------------*
DTS001 3256-99-FIM.                    EXIT.
DTS001*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ATUALIZAR HISTORICO DA OPERACAO                            *
      *----------------------------------------------------------------*
       3300-ACESSAR-DCOM7083           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE B6-ARGUMENTOS-ENTRADA.

           MOVE B6-OCORRENCIAS(1)      TO B6-COLUNAS-DA-TABELA.

           INITIALIZE B6-RETORNO
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE 14                     TO B6-CSIT-DESC-COML.
           MOVE WRK-TIMESTAMP          TO B6-HSIT-DESC-COML.
           MOVE 'TOTAL'                TO B6-INSTRUCAO.
           MOVE 'DCOM7083'             TO WRK-MODULO.

           CALL WRK-MODULO             USING B6-ARGUMENTOS-ENTRADA
                                             B6-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  B6-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '************* DCOM8110 *************'
               DISPLAY '*                                  *'
               DISPLAY '*          ERRO NO ACESSO          *'
               DISPLAY '*        AO MODULO DCOM7083        *'
               DISPLAY '*                                  *'
               DISPLAY '* CODIGO RETORNO: ' B6-COD-RETORNO
                                             '             *'
               DISPLAY '* DANO-OPER-DESC: ' B6-DANO-OPER-DESC
                                             '             *'
               DISPLAY '* NSEQ-OPER-DESC: ' B6-NSEQ-OPER-DESC
                                                  '        *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* DCOM8110 *************'
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO ACU-UPDTE-DCOMB0B6.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ATUALIZAR REGISTRO NA TABELA DE CONTROLE DE ROTINAS        *
      *----------------------------------------------------------------*
       3400-ATUALIZAR-DCOMB000         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 00-ARGUMENTOS-ENTRADA
                      00-RETORNO
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE 'DCOM8110'             TO 00-CPROG.
           MOVE WRK-JOBNAME            TO 00-CJOB.
           MOVE WRK-CHV-EMOVREAT       TO WRK-CHAVE-RESTART-AUX.
           MOVE ACU-UPDTE-DCOMB0B6     TO WRK-REST-UPDTE-B0B6.
           MOVE LENGTH OF WRK-CHAVE-RESTART
                                       TO 00-WCHAVE-RESTT-LEN.
           MOVE WRK-CHAVE-RESTART      TO 00-WCHAVE-RESTT-TEXT.
           MOVE 'TOTAL'                TO 00-INSTRUCAO.
           MOVE 'DCOM6503'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 00-ARGUMENTOS-ENTRADA
                                             00-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  00-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '*********** DCOM8110 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ERRO NO MODULO DCOM6503   *'
               DISPLAY '*    COD. RET. = ' 00-COD-RETORNO
                                            '          *'
               DISPLAY '*                              *'
               DISPLAY '*********** DCOM8110 ***********'
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     EFETIVAR ATUALIZACOES                                      *
      *----------------------------------------------------------------*
       3500-EFETUAR-COMMIT             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                COMMIT
           END-EXEC.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'COMMIT'           TO ERR-DBD-TAB
               MOVE 'COMMIT'           TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               MOVE  SQLCA             TO WRK-SQLCA
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE ZEROS                  TO ACU-COMMIT.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS FINAIS                                       *
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-UPDTE-DCOMB0B6      GREATER ZEROS
               PERFORM 4100-EXCLUIR-DCOMB000
               PERFORM 3500-EFETUAR-COMMIT
           END-IF.

           PERFORM 4200-EMITIR-DISPLAY-TOTAIS.

           CLOSE EMOVREAT
                 SOPERAC.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DELETAR REGISTRO NA TABELA DE CONTROLE DE ROTINAS          *
      *----------------------------------------------------------------*
       4100-EXCLUIR-DCOMB000           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 00-ARGUMENTOS-ENTRADA
                      00-RETORNO.

           MOVE 'DCOM8110'             TO 00-CPROG.
           MOVE WRK-JOBNAME            TO 00-CJOB.
           MOVE 'DCOM6504'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 00-ARGUMENTOS-ENTRADA
                                             00-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  00-COD-RETORNO          NOT EQUAL ZEROS
               DISPLAY '*********** DCOM8110 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ERRO NO MODULO DCOM6504   *'
               DISPLAY '*    COD. RET. = ' 00-COD-RETORNO
                                            '          *'
               DISPLAY '*                              *'
               DISPLAY '*********** DCOM8110 ***********'
               MOVE WRK-MODULO         TO ERR-MODULO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     EMITIR TOTAIS DOS CONTADORES                               *
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY-TOTAIS      SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-EMOVREAT     TO WRK-MASCARA.

           DISPLAY '**************** DCOM8110 ****************'.
           DISPLAY '*                                        *'.
           DISPLAY '* REG. LIDOS EMOVREAT......: ' WRK-MASCARA
                                                           ' *'.
           MOVE ACU-GRAVA-SOPERAC      TO WRK-MASCARA.

           DISPLAY '* REG. GRAVADOS SOPERAC....: ' WRK-MASCARA
                                                           ' *'.
           MOVE ACU-UPDTE-DCOMB0B6     TO WRK-MASCARA.

           DISPLAY '* REG. ATUALIZADOS DCOMB0B6: ' WRK-MASCARA
                                                           ' *'.
DTS001     MOVE ACU-OPERAC-NPR         TO WRK-MASCARA.
DTS001
DTS001     DISPLAY '* QUANTIDADE DE OPERAC NPR.: ' WRK-MASCARA
DTS001                                                     ' *'.
DTS001     MOVE ACU-NPR-AGRIC          TO WRK-MASCARA.
DTS001
DTS001     DISPLAY '*    QTDE NPR AGRICULTURA..: ' WRK-MASCARA
DTS001                                                     ' *'.
DTS001     MOVE ACU-NPR-PECUA          TO WRK-MASCARA.
DTS001
DTS001     DISPLAY '*    QTDE NPR PECUARIA.....: ' WRK-MASCARA
                                                           ' *'.
           DISPLAY '*                                        *'.
           DISPLAY '**************** DCOM8110 ****************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TRATAMENTO DE ERROS                                        *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM8110'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             WRK-SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
