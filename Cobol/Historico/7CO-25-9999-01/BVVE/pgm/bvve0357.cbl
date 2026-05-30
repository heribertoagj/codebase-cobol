      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     BVVE0357.
       AUTHOR.         FERNANDA RODRIGUES.
      *================================================================*
      *                     B S I   T E C N O L O G I A                *
      *----------------------------------------------------------------*
      *    ANALISTA....: FERNANDA RODRIGUES   -   BSI TECNOLOGIA       *
      *    DATA........: MARCO/2017                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: TRAVA AUTOMATICA - AGENDA FINANCEIRA REDE     *
      *                  SELECIONAR APENAS REG DE DETALHE DESPREZANDO  *
      *                  HEADER/TRAILLER. ADEQUAR A FORMATACAO DO CPF  *
      *                  A AGENDA CIP.                                 *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME        IN/OUT      INCLUDE               *
      *                AGENREDE      INPUT       INTERNO               *
      *                BASEREDE      OUTPUT      I#BVVETN              *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C -  AREA PARA TRATAMENTO DE ERRO PELA BRAD7100      *
DPS01 *    BVVEW044 -  AREA PARA MODULO BVVE044B/DE-PARA DE BANDEIRAS  *
.     *    BVVEW000 -  AREA ERRO MODULO BVVE044B/DE-PARA DE BANDEIRAS  *
.     *    I#FRWKGE - COMMAREA FRWK1999 (LOG DE ERRO )                 *
.     *    I#FRWKAR - COMMAREA FRWK1999 (ERRO ARQUIV0)                 *
DPS01 *    I#FRWDBD - COMMAREA FRWK1999 (ERRO DB2)                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 -  TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
DPS01 *    BVVE044B -  ROTINA PARA TRATAMENTO DE-PARA DE BANDEIRAS     *
.     *    BRAD0450 - ROTINA PARA ABENDAR O PROGRAMA                   *
.     *    CKRS1000 - ROTINA DE USO GERAL                              *
.     *    CKRS0105 - REALIZA CONEXAO/DESCONEXAO RRS                   *
DPS01 *    FRWK2999 - TRATAMENTO DE ERROS - BOOK I#FRWKGE              *
      *================================================================*
      *    DATA       AUTOR     DESCRICAO                              *
      *----------------------------------------------------------------*
190426*    26/04/2019 SIVEK    .INCLUIR BANDEIRAS AMEX, ELO E HIPER.   *
DPS01 *    06/08/2019 DESIREE  .INCLUIR TRATAMENTO DE-PARA DE BANDEIRAS*
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS          COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT  AGENREDE    ASSIGN  TO          UT-S-AGENREDE
                       FILE    STATUS  IS          WRK-FS-AGENREDE.

           SELECT  BASEREDE    ASSIGN  TO          UT-S-BASEREDE
                       FILE    STATUS  IS          WRK-FS-BASEREDE.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT :  AGENDA FINANCEIRA REDE                             *
      *----------------------------------------------------------------*
       FD  AGENREDE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-AGENREDE                 PIC  X(240).

      *----------------------------------------------------------------*
      *    OUTPUT:  AGENDA FINANCEIRA REDE                             *
      *             FORMATACAO DO CPF ADEQUADA AO LAYOUT CIP O
      *----------------------------------------------------------------*
       FD  BASEREDE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-BASEREDE                 PIC  X(280).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '*** BVVE0357 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*
       77  ACU-LIDOS-AGENREDE          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-BASEREDE          PIC  9(009) COMP-3  VALUE ZEROS.

       77  ACU-DESPR-NAODETALHE        PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-DESPR-BANDEIRA          PIC  9(009) COMP-3  VALUE ZEROS.

       77  ACU-SUBROT-BVVE040B         PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DE FILE-STATUS ***'.
      *----------------------------------------------------------------*
       77  WRK-FS-AGENREDE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-BASEREDE             PIC  X(002)         VALUE SPACES.

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
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

       01  WRK-CPF-AUX.
           05 FILLER                   PIC  X(003)         VALUE SPACES.
           05 WRK-CPF-NRO-AUX          PIC  9(009)         VALUE ZEROS.
           05 WRK-CPF-CTRL-AUX         PIC  9(002)         VALUE ZEROS.

       01  WRK-CPF-CIP                 PIC  9(014)         VALUE ZEROS.
       01  FILLER              REDEFINES WRK-CPF-CIP.
           05 WRK-CPF-NRO-CIP          PIC  9(009).
           05 WRK-CPF-CTRL-CIP         PIC  9(002).
           05 WRK-CPF-ZEROS-CIP        PIC  9(003).

DPS01  01  WRK-BANDEIRA-5-ALF          PIC  X(005)   VALUE SPACES.
.      01  FILLER          REDEFINES   WRK-BANDEIRA-5-ALF.
.          05  WRK-BANDEIRA-5-NUM-3    PIC  9(003).
.          05  FILLER                  PIC  X(002).
.
.      01  WRK-BANDEIRA-3-NUM          PIC  9(003)         VALUE ZEROS.
.      01  FILLER          REDEFINES   WRK-BANDEIRA-3-NUM.
.          05  WRK-BANDEIRA-3-ALF      PIC  X(003).
.
.      01  WRK-BAND-CORPORATIVA        PIC  9(003)   VALUE  ZEROS.
.      01  WRK-BAND-CIP-SLC            PIC  9(003)   VALUE  ZEROS.
.
.      01  WRK-ACHOU-BANDEIRA          PIC  X(001)   VALUE  SPACES.
.
.      01 WRK-REDE-BANDEIRA-ANT        PIC  X(001)   VALUE  ZEROS.
.
.      01  WRK-MODULO                  PIC  X(008)   VALUE SPACES.
.      01  WRK-BVVE0357                PIC  X(008)   VALUE 'BVVE0357'.
DPS01  01  WRK-FRWK2999                PIC  X(008)   VALUE 'FRWK2999'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO ARQUIVO AGENREDE ***'.
      *----------------------------------------------------------------*
       01  WRK-REDE-REGISTRO.
           05 FILLER                   PIC  X(007)         VALUE SPACES.
           05 WRK-REDE-TP-REG          PIC  9(001)         VALUE ZEROS.
           05 FILLER                   PIC  X(064)         VALUE SPACES.
           05 WRK-REDE-BANDEIRA        PIC  X(001)         VALUE ZEROS.
           05 FILLER                   PIC  X(106)         VALUE SPACES.
           05 WRK-REDE-CNPJ-CPF        PIC  X(014)         VALUE SPACES.
           05 WRK-REDE-TP-PSSOA        PIC  X(001)         VALUE SPACES.
           05 FILLER                   PIC  X(046)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO ARQUIVO BASEREDE ***'.
      *----------------------------------------------------------------*
       COPY I#BVVETN.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO - FILE-STATUS ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(018)         VALUE
               ' - FILE STATUS =  '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY I#BRAD7C.

DPS01 *----------------------------------------------------------------*
.     *                   '** AREA DA BRAD0450 **                      *
.     *----------------------------------------------------------------*
.
.      01  WRK-B0450-ABEND             PIC S9(004)  COMP   VALUE +1111.
.      01  WRK-B0450-DUMP              PIC  X(001)         VALUE 'S'.
.
.     *----------------------------------------------------------------*
.     *                     ** INFORMACOES DE ERRO - ARQ **            *
.     *----------------------------------------------------------------*
.
.      01  WRK-AREA-ERRO-ARQUIVO.
.          COPY I#FRWKAR.
.
.     *----------------------------------------------------------------*
.     *    'AREA DE COMUNICACAO DO FRWK2999 (LOG DE ERRO)'.            *
.     *----------------------------------------------------------------*
.      01  WRK-AREA-ERRO.
.          COPY 'I#FRWKGE'.
.          05 WRK-BLOCO-INFO-ERRO.
.            10 WRK-CHAR-INFO-ERRO     PIC  X(01) OCCURS 0 TO 526 TIMES
.                                      DEPENDING ON FRWKGHEA-TAM-DADOS.
.
.     *----------------------------------------------------------------*
.      01  FILLER                      PIC  X(050)         VALUE
.          'AREA DE TRATAMENTO DE ERRO DB2 (API-ERRO)'.
.     *----------------------------------------------------------------*
.      01  WRK-AREA-ERRO-DB2.
.          COPY 'I#FRWKDB'.
.
.      01  WRK-AREA-ERRO-MODULO.
.          COPY I#FRWKMD.
.
.      01  WRK-AREA-ERRO-LIVRE.
.          COPY 'I#FRWKLI'.
.
.
.      01  WRK-AREA-RESTART.
.          05 WRK-REST-ARQENT01        PIC  9(09)  COMP-3  VALUE ZEROS.
.          05 WRK-REST-UPDATE          PIC  9(09)  COMP-3  VALUE ZEROS.
.
.     *----------------------------------------------------------------*
.     *     AREA  COPY  PARA BVVE044B - ROTINA DE-PARA DE BANDEIRAS    *
.     *----------------------------------------------------------------*
.      01  WRK-MOD-BVVE044B            PIC X(008) VALUE 'BVVE044B'.
.
.      01  WRK-BVVE044B.
.          COPY BVVEW044.
.          COPY BVVEW000.
.
.     *----------------------------------------------------------------*
.     *    AREA COPY PARA CKRS0100 - AREA CHECKPOINT                  '.
.     *----------------------------------------------------------------*
.
DPS01      COPY I#CKRS01.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVE0357 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE  DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

DPS01      CALL 'CKRS1000'.

DPS01      INITIALIZE  FRWKGERR-REGISTRO
.                      FRWKGHEA-REGISTRO
.                      FRWKGARQ-REGISTRO
.                      FRWKGMOD-REGISTRO.
.
DPS01      MOVE  SPACES                    TO  WRK-REDE-BANDEIRA-ANT.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-FS-AGENREDE     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

DPS01      PERFORM 1050-INICIAR-COMMIT-RESTART.

           OPEN INPUT AGENREDE
               OUTPUT BASEREDE.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

DPS01 *----------------------------------------------------------------*
.      1050-INICIAR-COMMIT-RESTART     SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE    '1050-INICIAR-COMMIT-RESTART'
.                                          TO
.                                          FRWKGHEA-IDEN-PARAGRAFO.
.
.          INITIALIZE                              CKRS01-INTERFACE.
.
.          MOVE    'I'                     TO      CK01-FUNCAO.
.          MOVE    'DB2'                   TO      CK01-ID-DB2.
.          MOVE    SPACES                  TO      CK01-PLAN.
.          MOVE    LENGTH OF WRK-AREA-RESTART
.                                          TO      CK01-TAM-AREA-RESTART
.          MOVE    WRK-AREA-RESTART        TO      CK01-AREA-RESTART
.                                              (1:CK01-TAM-AREA-RESTART)
.
.          PERFORM 1060-ACESSAR-CKRS0100.
.
.          IF      CK01-STATUS             EQUAL   'REST'
.                  MOVE CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)
.                                          TO      WRK-AREA-RESTART
.          END-IF.
.
.     *----------------------------------------------------------------*
.      1050-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.     * ACESSO AO MODULO CKRS0100.                                     *
.     *----------------------------------------------------------------*
.      1060-ACESSAR-CKRS0100           SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE    '1060-ACESSAR-CKRS0100' TO
.                                          FRWKGHEA-IDEN-PARAGRAFO.
.
.          MOVE    'CKRS0100'              TO      WRK-MODULO.
.
.          CALL    WRK-MODULO              USING   CKRS01-INTERFACE.
.
.          IF     (RETURN-CODE     NOT     EQUAL   ZEROS)  OR
.                 (CK01-CODIGO-RETORNO NOT EQUAL   ZEROS)
.                 IF  (RETURN-CODE         EQUAL   12)     OR
.                     (CK01-CODIGO-RETORNO EQUAL   12)
.                      GOBACK
.                 ELSE
.                     SET ERRO-MODULO      TO      TRUE
.                     MOVE CK01-CODIGO-RETORNO
.                                          TO      FRWKGMOD-COD-RETORNO
.                     MOVE 'CKRS0100'      TO      FRWKGMOD-NOME-MODULO
.                     IF   CK01-CODIGO-RETORNO
.                                          EQUAL    08
.                          MOVE CK01-TEXTO-MENSAGEM(01:08)
.                                          TO      FRWKGMOD-COD-MENSAGEM
.                          PERFORM 9999-ROTINA-ERRO
.                     ELSE
.                          MOVE 'BVVE0357' TO     FRWKGMOD-COD-MENSAGEM
.                          DISPLAY '************** BVVE0357 ***********'
.                          DISPLAY '* CK01-CODIGO-RETORNO : '
.                             CK01-CODIGO-RETORNO '                *'
.                          DISPLAY '* CK01-CODIGO-MENSAGEM: '
.                             CK01-CODIGO-MENSAGEM '          *'
.                          DISPLAY '************** BVVE0357 ***********'
.                          PERFORM 9999-ROTINA-ERRO
.                     END-IF
.                 END-IF
.          END-IF.
.
.     *----------------------------------------------------------------*
.      1060-99-FIM.                    EXIT.
DPS01 *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-AGENREDE

           PERFORM 1130-TESTAR-FS-BASEREDE.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-AGENREDE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AGENREDE         NOT EQUAL   ZEROS
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  WRK-FS-AGENREDE   TO  WRK-FILE-STATUS
               MOVE 'AGENREDE'         TO  WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-BASEREDE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-BASEREDE         NOT EQUAL   ZEROS
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  WRK-FS-BASEREDE   TO  WRK-FILE-STATUS
               MOVE 'BASEREDE'         TO  WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-AGENREDE.

           IF  WRK-FS-AGENREDE         EQUAL '10'
               DISPLAY '**************** BVVE0357 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*         ARQUIVO AGENREDE VAZIO         *'
               DISPLAY '*        PROCESSAMENTO FINALIZADO        *'
               DISPLAY '*                                        *'
               DISPLAY '**************** BVVE0357 ****************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-AGENREDE               SECTION.
      *----------------------------------------------------------------*

           READ AGENREDE               INTO WRK-REDE-REGISTRO

           IF  WRK-FS-AGENREDE         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-AGENREDE.

           ADD  1                      TO ACU-LIDOS-AGENREDE.

           IF  WRK-REDE-TP-REG         NOT EQUAL 3
               ADD  1                  TO ACU-DESPR-NAODETALHE
               GO TO 2100-LER-AGENREDE
           END-IF.


      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

DPS01      IF WRK-REDE-BANDEIRA NOT EQUAL WRK-REDE-BANDEIRA-ANT
.             PERFORM 3500-TRATA-DE-PARA-BANDEIRAS
.             MOVE   WRK-REDE-BANDEIRA TO WRK-REDE-BANDEIRA-ANT
.          END-IF.
.
.          IF  WRK-ACHOU-BANDEIRA EQUAL 'N'
.              ADD  1                  TO ACU-DESPR-BANDEIRA
DPS01      ELSE
               PERFORM 3100-GRAVAR-BASEREDE
DPS01      END-IF.

           PERFORM 2100-LER-AGENREDE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-GRAVAR-BASEREDE            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-REDE-REGISTRO      TO BVVETN-REGISTRO

           IF  WRK-REDE-TP-PSSOA       EQUAL 'F'
               MOVE WRK-REDE-CNPJ-CPF   TO WRK-CPF-AUX
               MOVE WRK-CPF-NRO-AUX     TO WRK-CPF-NRO-CIP
               MOVE WRK-CPF-CTRL-AUX    TO WRK-CPF-CTRL-CIP
               MOVE ZEROS               TO WRK-CPF-ZEROS-CIP

               MOVE WRK-CPF-CIP         TO BVVETN-CNPJ-CPF
           END-IF.

DPS01      MOVE BVVEW044-S-CBANDE-VISAO TO WRK-BANDEIRA-3-NUM.
DPS01      MOVE WRK-BANDEIRA-3-ALF      TO BVVETN-BAND-CIP.

DPS01      MOVE WRK-BAND-CIP-SLC        TO WRK-BANDEIRA-3-NUM.
DPS01      MOVE WRK-BANDEIRA-3-ALF      TO BVVETN-BAND-CIP.


           WRITE FD-BASEREDE           FROM BVVETN-REGISTRO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1130-TESTAR-FS-BASEREDE.

           ADD 1                       TO ACU-GRAVA-BASEREDE.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

DPS01 *----------------------------------------------------------------*
.     * ROTINA PARA TRATAMENTO DE-PARA DE BANDEIRAS                    *
.     *----------------------------------------------------------------*
.      3500-TRATA-DE-PARA-BANDEIRAS    SECTION.
.     *----------------------------------------------------------------*
.
.          PERFORM 3510-PESQUISA-BAND-CORPORATIVA
.
.          IF WRK-ACHOU-BANDEIRA EQUAL 'S'
.             PERFORM 3520-PESQUISA-BAND-CIP-SLC
.          END-IF
.
.          .
.     *----------------------------------------------------------------*
.      3500-99-FIM.                    EXIT.
DPS01 *----------------------------------------------------------------*

DPS01 *----------------------------------------------------------------*
.     * ROTINA PESQUISA BANDEIRA CORPORATIVA                           *
.     *----------------------------------------------------------------*
.      3510-PESQUISA-BAND-CORPORATIVA  SECTION.
.     *----------------------------------------------------------------*
.
.     *--- COM BVVETK-BANDEIRA DO ARQUIVO AGENREDE (REDECARD) BUSCA
.     *--- BANDEIRA CORPORATIVA CAMPO CBAND-VISAO DA TAB.BVVEB044
.
.          INITIALIZE   WRK-BVVE044B
.          MOVE SPACES                 TO WRK-ACHOU-BANDEIRA
.          MOVE ZEROS                  TO WRK-BAND-CORPORATIVA
.
.          MOVE 'C'                    TO  BVVEW044-E-C-ATIVIDADE
.          MOVE '001'                  TO  BVVEW044-E-C-FUNCAO
.
.     *-- CTPO-VISAO-BANDE = 2 (ADQUIRENTE) CAMPO DA TABELA
.     *-- DB2 DB2PRD.TTPO_VISAO_BANDE
.          MOVE 2                      TO  BVVEW044-E-CTPO-VISAO-BANDE
.
.     *-- CTPO-AMBTE-EXTER = 2 (REDECARD)
.          MOVE 2                      TO  BVVEW044-E-CTPO-AMBTE-EXTER
.
.     *-- BVVETK-BANDEIRA DO ARQUIVO AGENREDE (REDECARD)
.          MOVE WRK-REDE-BANDEIRA      TO  BVVEW044-E-CBANDE-VISAO-EXTER
.
.     *--- SUBROTINA BVVE044B TRATAMENTO DE-PARA DA BANDEIRA
.          PERFORM 3600-ACESSA-BVVE044B
.
.          IF BVVEW000-COD-RETORNO OF WRK-BVVE044B  EQUAL ZEROS
.             MOVE  'S'                TO  WRK-ACHOU-BANDEIRA
.             MOVE  BVVEW044-S-CBANDE-VISAO
.                                      TO  WRK-BAND-CORPORATIVA
.          ELSE
.             IF  BVVEW000-COD-RETORNO OF WRK-BVVE044B EQUAL 15
.             AND BVVEW000-COD-ERRO OF WRK-BVVE044B    EQUAL '0100'
.                MOVE 'N'                 TO  WRK-ACHOU-BANDEIRA
.             ELSE
.                DISPLAY '***************** BVVE0357 ***************'
.                DISPLAY '*  ERRO SUBROTINA BVVE044B  C-001********'
.                DISPLAY '*  WRK-REDE-BANDEIRA    > '
.                    WRK-REDE-BANDEIRA
.                DISPLAY '*  BVVEW000-COD-RETORNO > '
.                    BVVEW000-COD-RETORNO OF WRK-BVVE044B
.                DISPLAY '*  BVVEW000-COD-RETORNO > '
.                    BVVEW000-COD-ERRO    OF WRK-BVVE044B
.                DISPLAY '*  BVVEW000-COD-RETORNO > '
.                    BVVEW000-COD-MENSAGEM OF WRK-BVVE044B
.                DISPLAY '***************** BVVE0357 ***************'
.                PERFORM 9999-ROTINA-ERRO
.             END-IF
.          END-IF
.          .
.     *----------------------------------------------------------------*
.      3510-99-FIM.                    EXIT.
DPS01 *----------------------------------------------------------------*

DPS01 *----------------------------------------------------------------*
.     * ROTINA PESQUISA BANDEIRA CIP SLC                               *
.     *----------------------------------------------------------------*
.      3520-PESQUISA-BAND-CIP-SLC      SECTION.
.     *----------------------------------------------------------------*
.
.     *--- COM BANDEIRA CORPORATIVA CAMPO CBAND-VISAO DA TAB.BVVEB044
.     *--- BUSCA A BANDEIRA CIP-SLC NA TAB.BVVEB044.RETORNA INFORMACAO
.     *--- NO CAMPO BVVEW044-E-CBANDE-VISAO-EXTER
.
.          INITIALIZE   WRK-BVVE044B
.          MOVE SPACES                 TO WRK-ACHOU-BANDEIRA
.          MOVE ZEROS                  TO WRK-BAND-CIP-SLC
.
.          MOVE 'C'                    TO  BVVEW044-E-C-ATIVIDADE
.          MOVE '002'                  TO  BVVEW044-E-C-FUNCAO
.
.     *-- CTPO-VISAO-BANDE = 2 (ADQUIRENTE) CAMPO DA TABELA
.     *-- DB2 DB2PRD.TTPO_VISAO_BANDE
.          MOVE 2                      TO  BVVEW044-E-CTPO-VISAO-BANDE
.
.     *-- CTPO-AMBTE-EXTER = 4 (CIP SLC)
.          MOVE 4                      TO  BVVEW044-E-CTPO-AMBTE-EXTER
.
.     *-- CBANDE-VISAO DA TAB.BVVEB044 - BAND.CORPORATIVA
.          MOVE WRK-BAND-CORPORATIVA   TO  BVVEW044-E-CBANDE-VISAO
.
.     *--- SUBROTINA BVVE044B TRATAMENTO DE-PARA DA BANDEIRA
.          PERFORM 3600-ACESSA-BVVE044B
.
.          IF BVVEW000-COD-RETORNO OF WRK-BVVE044B  EQUAL ZEROS
.             MOVE 'S'                 TO WRK-ACHOU-BANDEIRA
.             MOVE BVVEW044-S-CBANDE-VISAO-EXTER
.                                      TO  WRK-BANDEIRA-5-ALF
.             MOVE WRK-BANDEIRA-5-NUM-3
.                                      TO  WRK-BAND-CIP-SLC
.          ELSE
.             IF  BVVEW000-COD-RETORNO OF WRK-BVVE044B EQUAL 17
.             AND BVVEW000-COD-ERRO    OF WRK-BVVE044B EQUAL '0100'
.                MOVE 'N'              TO  WRK-ACHOU-BANDEIRA
.             ELSE
.                DISPLAY '***************** BVVE0357 ***************'
.                DISPLAY '*  ERRO SUBROTINA BVVE044B C-002 *********'
.                DISPLAY '*  WRK-BAND-CORPORATIVA > '
.                    WRK-BAND-CORPORATIVA
.                DISPLAY '*  BVVEW000-COD-RETORNO > '
.                    BVVEW000-COD-RETORNO OF WRK-BVVE044B
.                DISPLAY '*  BVVEW000-COD-RETORNO > '
.                    BVVEW000-COD-ERRO    OF WRK-BVVE044B
.                DISPLAY '*  BVVEW000-COD-RETORNO > '
.                    BVVEW000-COD-MENSAGEM OF WRK-BVVE044B
.                DISPLAY '***************** BVVE0357 ***************'
.                PERFORM 9999-ROTINA-ERRO
.             END-IF
.          END-IF
.          .
.     *----------------------------------------------------------------*
.      3520-99-FIM.                    EXIT.
DPS01 *----------------------------------------------------------------*

DPS01 *----------------------------------------------------------------*
.     * ROTINA BVVE044B TRATAMENTO DE-PARA DA BANDEIRA                 *
.     *----------------------------------------------------------------*
.      3600-ACESSA-BVVE044B            SECTION.
.     *----------------------------------------------------------------*
.
.          CALL  WRK-MOD-BVVE044B      USING WRK-BVVE044B
.
.          ADD  1                      TO ACU-SUBROT-BVVE040B
.
.          PERFORM 3900-EFETUAR-CHECKPOINT
.          .
.
.     *----------------------------------------------------------------*
.      3600-99-FIM.                    EXIT.
DPS01 *----------------------------------------------------------------*

DPS01 *----------------------------------------------------------------*
.     * EFETUAR CHECKPOINT RESTART.                                    *
.     *----------------------------------------------------------------*
.      3900-EFETUAR-CHECKPOINT         SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE    '3900-EFETUAR-CHECKPOINT'
.                                          TO
.                                          FRWKGHEA-IDEN-PARAGRAFO.
.
.          INITIALIZE                              CKRS01-INTERFACE.
.
.          MOVE    'P'                     TO      CK01-FUNCAO.
.          MOVE    'DB2'                   TO      CK01-ID-DB2.
.          MOVE    SPACES                  TO      CK01-PLAN.
.          MOVE    LENGTH                  OF      WRK-AREA-RESTART
.                                          TO      CK01-TAM-AREA-RESTART
.
.          MOVE    WRK-AREA-RESTART        TO      CK01-AREA-RESTART
.                                          (1:CK01-TAM-AREA-RESTART).
.
.          PERFORM 1060-ACESSAR-CKRS0100.
.
.     *----------------------------------------------------------------*
.      3900-99-FIM.                    EXIT.
DPS01 *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 5000-EMITIR-DISPLAY-FINAL.

DPS01      PERFORM 4500-FINALIZAR-COMMIT-REST

           CLOSE   AGENREDE
                   BASEREDE.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS

DPS01      MOVE  ZEROS                 TO RETURN-CODE

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

DPS01 *----------------------------------------------------------------*
.      4500-FINALIZAR-COMMIT-REST      SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE    '4500-FINALIZAR-COMMIT-REST'
.                                          TO
.                                          FRWKGHEA-IDEN-PARAGRAFO.
.          INITIALIZE                              CKRS01-INTERFACE.
.
.          MOVE    'F'                     TO      CK01-FUNCAO.
.          MOVE    'DB2 '                  TO      CK01-ID-DB2.
.          MOVE    SPACES                  TO      CK01-PLAN.
.
.          PERFORM 1060-ACESSAR-CKRS0100.
.
.     *----------------------------------------------------------------*
.      4500-99-FIM.                    EXIT.
DPS01 *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-EMITIR-DISPLAY-FINAL       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************* BVVE0357 ******************'.
           DISPLAY '*                                             *'.

           MOVE ACU-LIDOS-AGENREDE     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS AGENREDE   :   ' WRK-MASCARA
                                                               '  *'.
           MOVE ACU-GRAVA-BASEREDE     TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS BASEREDE:   ' WRK-MASCARA
                                                               '  *'.
           DISPLAY '*---------------------------------------------*'.
           DISPLAY '*                                             *'.
           MOVE ACU-DESPR-NAODETALHE   TO WRK-MASCARA.
           DISPLAY '* REG. DESPR. HEADER/TRAILLER:   ' WRK-MASCARA
                                                               '  *'.
           MOVE ACU-DESPR-BANDEIRA     TO WRK-MASCARA.
           DISPLAY '* REG. DESPR. BANDEIRA       :   ' WRK-MASCARA
                                                               '  *'.
           DISPLAY '*                                             *'.
DPS01      DISPLAY '*---------------------------------------------*'.
.          DISPLAY '*                                             *'.
.          MOVE ACU-SUBROT-BVVE040B    TO WRK-MASCARA.
.          DISPLAY '* REG.TRATADOS SUBROT. BVVE040B :' WRK-MASCARA
.                                                              '  *'.
DPS01      DISPLAY '*                                             *'.
           DISPLAY '******************* BVVE0357 ******************'.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE0357'             TO ERR-PGM.

DPS01      MOVE  12                    TO  FRWKGERR-COD-RETORNO
.          MOVE  WRK-BVVE0357          TO  FRWKGHEA-NOME-PROGRAMA
.
.          EVALUATE FRWKGHEA-TIPO-ERRO
.
.            WHEN  'AR'
.               SET   FRWKGERR-FSTATUS-INCOR    TO  TRUE
.               MOVE  FRWKGARQ-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS
.               MOVE  WRK-AREA-ERRO-ARQUIVO     TO  WRK-BLOCO-INFO-ERRO
.
.            WHEN  'MO'
.               SET   FRWKGERR-COD-RETOR-INCOR  TO  TRUE
.               MOVE  FRWKGMOD-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS
.               MOVE  WRK-AREA-ERRO-MODULO      TO  WRK-BLOCO-INFO-ERRO
.
.            WHEN  'LI'
.               SET   FRWKGERR-TIPO-ERRO-INCOR  TO  TRUE
.               MOVE  FRWKGLIV-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS
.               MOVE  WRK-AREA-ERRO-LIVRE       TO  WRK-BLOCO-INFO-ERRO
.
.            WHEN  'DB'
.               MOVE  FRWKGDB2-SQLCODE          TO  FRWKGDB2-SQLCODE
.               MOVE  FRWKGDB2-SQLSTATE         TO  FRWKGDB2-SQLSTATE
.               SET   FRWKGERR-SQLCODE-INCOR    TO  TRUE
.               MOVE  FRWKGDB2-TAM-LAYOUT       TO  FRWKGHEA-TAM-DADOS
.               MOVE  WRK-AREA-ERRO-DB2         TO  WRK-BLOCO-INFO-ERRO
.
.          END-EVALUATE
.
.          CALL  WRK-FRWK2999          USING  WRK-AREA-ERRO
.
.          CALL  'BRAD0450'            USING  WRK-B0450-ABEND
DPS01                                         WRK-B0450-DUMP.



           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
