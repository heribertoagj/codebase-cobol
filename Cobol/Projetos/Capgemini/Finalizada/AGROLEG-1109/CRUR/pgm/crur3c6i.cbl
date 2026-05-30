      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CRUR3C6I.
       AUTHOR.     EVANDRO GUIMARAES.
      *================================================================*
      *             C A P G E M I N I  -  S I S T E M A S              *
      *----------------------------------------------------------------*
      *    PROGRAMA....: CRUR3C6I                                      *
      *    ANALISTA....: EVANDRO GUIMARAES       - CAPGEMINI - ALPHA   *
      *    DATA........: 12/2017                                       *
      *----------------------------------------------------------------*
      *    OBJETIVO....: GRAVAR RESPONSABILIDADES RETORNADAS           *
      *                  NA COR0006R1                                  *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TRESP_PSSOA_BACEN                             CRURB0WE      *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#FRWKGE - COMMAREA FRWK1999 (LOG DE ERRO)                  *
      *    I#FRWKCI - COMMAREA FRWK1999 (LOG DE ERROS CICS)            *
      *    I#FRWKLI - COMMAREA FRWK1999 (LOG DE ERROS LIVRES)          *
      *    I#FRWKDB - COMMAREA FRWK1999 (LOG DE ERROS DB2)             *
      *    CRURWE12 - AREA DE COMUNICACAO COM CHAMADOR                 *
      *================================================================*
ID6416* ALTERACOES..: ID6416/ID6440                                    *
ID6416* NOME........: FABIO CASTILHO - BRQ                             *
ID6416* DATA........: 11/07/2018                                       *
ID6416* DESCRICAO...: FAZER TRATAMENTO PARA CONSIDERAR RESPONSABILIDA- *
ID6416*               DES SOMENTE REFERENTE A CREDITO RURAL, CASO A    *
ID6416*               LINHA DE CREDITO NAO ESTIVER CADASTRADA EFETUAR  *
ID6416*               A INCLUSAO NA BASE TLIN_CREDT_BACEN.             *
ID6416*                                                                *
ID6416*           *** TIPOS DE LINHA DE CREDITO:                       *
ID6416*               - 001 - PRONAF                                   *
ID6416*               - 050 - PRONAMP                                  *
ID6416*               - 070 - FUNCAFE                                  *
ID6416*               - 999 - FINANCIAMENTO SEM VINCULO A PROGRAMA     *
ID6416*                       ESPECIFICO                               *
BR0521*================================================================*
BR0521* DATA       AUTOR             DESCRICAO / MANUTENCAO            *
BR0521*----------------------------------------------------------------*
BR0521* 26/05/2021 BRQ               SICOR 5.02                        *
BR0521*================================================================*
WIP001*================================================================*
-     * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
-     *----------------------------------------------------------------*
-     * 26/10/2022 BRQ               SICOR 5.05                        *
WIP001*================================================================*
7C5.10*================================================================*
.     * ANALISTA REPONSAVEL: EDSON/NIKSON                              *
.     * DATA...............: AGOSTO/2025                               *
.     * OBJETIVOS..........: FLASH - AGRO7-000 - FORMATAR O CPF COM    *
.     *                      BRANCOS A DIREITA                         *
7C5.10*================================================================*
      *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** INICIO DA WORKING CRUR3C6I **'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** AREA WRK-AUXILIARES        **'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-FIM-CRURB0WD         PIC  X(001)     VALUE SPACES.
           05 WRK-FIM-CRURB0WE         PIC  X(001)     VALUE SPACES.
           05 WRK-FRWK1999             PIC  X(008)     VALUE 'FRWK1999'.
           05 WRK-POS                  PIC S9(005) COMP-3 VALUE ZEROS.
           05 WRK-NRETOR-BACEN-RESP    PIC S9(005)     VALUE ZEROS.
           05 WRK-INCLUSAO-OK          PIC  X(001)     VALUE 'N'.
           05 WRK-PROGRAMA             PIC  X(008)     VALUE 'CRUR3C6I'.
           05 WRK-DECM-8               PIC  9(008)     VALUE ZEROS.
           05 WRK-DECM-11              PIC  9(011)     VALUE ZEROS.

       01  WRK-CLIN-X                  PIC  X(004)         VALUE SPACES.
       01  FILLER                      REDEFINES     WRK-CLIN-X.
           05  WRK-CLIN-9              PIC  9(004).

      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** REDEFINICOES HEXAVISIO     **'.
      *----------------------------------------------------------------*

       01  WRK-VALOR                   PIC  X(019)         VALUE SPACES.
       01  FILLER            REDEFINES WRK-VALOR.
           03  FILLER                  PIC  X(004).
           03  WRK-VALOR-DB2           PIC  9(013)V99.

       01  WRK-ZD8                     PIC  9(008)     VALUE ZEROS.
       01  FILLER REDEFINES WRK-ZD8.
           03  FILLER                  PIC  X(007).
           03  WRK-CH1                 PIC  X(001).
       01  FILLER REDEFINES WRK-ZD8.
           03  FILLER                  PIC  X(007).
           03  WRK-ZD1                 PIC  9(001).
       01  FILLER REDEFINES WRK-ZD8.
           03  FILLER                  PIC  X(006).
           03  WRK-CH2                 PIC  X(002).
       01  FILLER REDEFINES WRK-ZD8.
           03  FILLER                  PIC  X(006).
           03  WRK-ZD2                 PIC  9(002).
       01  FILLER REDEFINES WRK-ZD8.
           03  FILLER                  PIC  X(005).
           03  WRK-CH3                 PIC  X(003).
       01  FILLER REDEFINES WRK-ZD8.
           03  FILLER                  PIC  X(005).
           03  WRK-ZD3                 PIC  9(003).
       01  FILLER REDEFINES WRK-ZD8.
           03  FILLER                  PIC  X(004).
           03  WRK-CH4                 PIC  X(004).
       01  FILLER REDEFINES WRK-ZD8.
           03  FILLER                  PIC  X(004).
           03  WRK-ZD4                 PIC  9(004).
       01  FILLER REDEFINES WRK-ZD8.
           03  FILLER                  PIC  X(003).
           03  WRK-CH5                 PIC  9(005).
       01  FILLER REDEFINES WRK-ZD8.
           03  WRK-CH8                 PIC  X(008).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** AUXILIARES DE DATA         **'.
      *----------------------------------------------------------------*

       01  WRK-DATA-AMD.
           03  WRK-DATA-AAAA           PIC  X(004) VALUE SPACES.
           03  WRK-DATA-MM             PIC  X(002) VALUE SPACES.
           03  WRK-DATA-DD             PIC  X(002) VALUE SPACES.

       01  WRK-DATA-DB2.
           03  WRK-DATA-DD             PIC  X(002) VALUE SPACES.
           03                          PIC  X(001) VALUE '.'.
           03  WRK-DATA-MM             PIC  X(002) VALUE SPACES.
           03                          PIC  X(001) VALUE '.'.
           03  WRK-DATA-AAAA           PIC  X(004) VALUE SPACES.

4.11   01  WRK-CPF-CNPJ-15.
4.11       05  WRK-ZEROS               PIC  9(001) VALUE ZEROS.
4.11       05  WRK-CPF-CNPJ-14         PIC  9(014) VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** REFORMATACAO CPF/CNPJ      **'.
      *----------------------------------------------------------------*

       01  WRK-CPF-BACEN.
           05 WRK-CPF                  PIC  X(009)     VALUE SPACES.
           05 WRK-CONTROLE-CPF         PIC  X(002)     VALUE SPACES.
7C5.10     05 FILLER                   PIC  X(003)     VALUE SPACES.
      *
       01  WRK-CNPJ-BACEN.
           05 WRK-CNPJ                 PIC  X(008)     VALUE SPACES.
           05 WRK-FILIAL-CNPJ          PIC  X(004)     VALUE SPACES.
           05 WRK-CONTROLE-CNPJ        PIC  X(002)     VALUE SPACES.
      *
       01  WRK-CPF-CNPJ-CRURB0WE-A.
           05 WRK-CPF-CNPJ-CRURB0WE    PIC  X(009)     VALUE SPACES.
           05 WRK-CPF-CNPJ-CRURB0WE-N  REDEFINES
              WRK-CPF-CNPJ-CRURB0WE    PIC  9(009).
           05 WRK-FILIAL-CRURB0WE      PIC  X(004)     VALUE SPACES.
           05 WRK-FILIAL-CRURB0WE-N    REDEFINES
              WRK-FILIAL-CRURB0WE      PIC  9(004).
           05 WRK-CTRL-CRURB0WE        PIC  X(002)     VALUE SPACES.
           05 WRK-CTRL-CRURB0WE-N      REDEFINES
              WRK-CTRL-CRURB0WE        PIC  9(002).

BR0521 01  WRK-CNPJ-ENT-RESP-OR        PIC 9(014)     VALUE ZEROS.
.      01  WRK-CNPJ-ENT-RESP-OR-R      REDEFINES WRK-CNPJ-ENT-RESP-OR.
.          05 WRK-CNPJ-N               PIC 9(009).
.          05 WRK-FILIAL-N             PIC 9(005).
BR0521     05 WRK-CTRL-N               PIC 9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** AREA DE NULO CRURB0WE      **'.
      *----------------------------------------------------------------*
       01  WRK-AREA-NULOS.
           05 WRK-CUSUAR-MANUT-NULL    PIC S9(04) COMP VALUE ZEROS.
           05 WRK-CUSUAR-MANUT-N       PIC S9(04) COMP VALUE ZEROS.
           05 WRK-HMANUT-REG-N         PIC S9(04) COMP VALUE ZEROS.
           05 WRK-CTPO-PSSOA-COOP-NULL PIC S9(04) COMP VALUE ZEROS.
4.11       05 WRK-CDOCTO-PSSOA-COOP-NULL
4.11                                   PIC S9(04) COMP VALUE ZEROS.
4.11       05 WRK-CTPO-PSSOA-BNEFC-NULL
4.11                                   PIC S9(04) COMP VALUE ZEROS.
4.11       05 WRK-CDOCTO-PSSOA-BNEFC-NULL
4.11                                   PIC S9(04) COMP VALUE ZEROS.
BR4.12     05 WRK-CSUB-PROG-RURAL-NULL PIC S9(04) COMP VALUE ZEROS.
5.0        05 WRK-CFLIAL-BASE-MUT-NULL PIC S9(04) COMP VALUE ZEROS.
5.0        05 WRK-CCNPJ-BASE-MUTU-NULL PIC S9(04) COMP VALUE ZEROS.
5.0        05 WRK-CCTRL-BASE-MUTU-NULL PIC S9(04) COMP VALUE ZEROS.
5.0        05 WRK-NREFT-BACEN-INV-NULL PIC S9(04) COMP VALUE ZEROS.
5.0        05 WRK-VPCELA-INVES-RU-NULL PIC S9(04) COMP VALUE ZEROS.
BR0521     05 WRK-CCNPJ-ENTID-RESP-NULL  PIC S9(04) COMP VALUE ZEROS.
.          05 WRK-CFLIAL-CNPJ-ENTID-NULL PIC S9(04) COMP VALUE ZEROS.
.          05 WRK-CCTRL-CNPJ-ENTID-NULL  PIC S9(04) COMP VALUE ZEROS.
.          05 WRK-CCNPJ-PARTC-NULL       PIC S9(04) COMP VALUE ZEROS.
.          05 WRK-CFLIAL-CNPJ-PARTC-NULL PIC S9(04) COMP VALUE ZEROS.
BR0521     05 WRK-CCTRL-CNPJ-PARTC-NULL  PIC S9(04) COMP VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** EMPREENDIMENTO SICOR       **'.
      *----------------------------------------------------------------*

       01  FILLER.
           03  WRK-EMPREEND-SICOR      PIC X(014)      VALUE SPACES.
           03  FILLER  REDEFINES  WRK-EMPREEND-SICOR.
               05  WRK-ATIV-SICOR      PIC X(001).
               05  WRK-FINAL-SICOR     PIC X(001).
               05  WRK-MODALID-SICOR   PIC X(002).
               05  WRK-PRODUTO-SICOR   PIC X(004).
               05  WRK-VARIED-SICOR    PIC X(003).
               05  WRK-CONSORC-SICOR   PIC X(001).
               05  WRK-CESTA-SICOR     PIC X(001).
               05  WRK-ZONEAM-SICOR    PIC X(001).

STE001     03  WRK-SIST-PROD-SICOR     PIC X(013)      VALUE SPACES.
           03  FILLER  REDEFINES  WRK-SIST-PROD-SICOR.
               05  WRK-TPAGROP-SICOR   PIC X(001).
               05  WRK-TPINTEG-SICOR   PIC X(001).
               05  WRK-GRAOSEM-SICOR   PIC X(001).
               05  WRK-TPIRRIG-SICOR   PIC X(002).
               05  WRK-TPCULTV-SICOR   PIC X(002).
               05  WRK-FASECIC-SICOR   PIC X(002).
WIP001         05  FILLER              PIC X(002).
STE001         05  WRK-OUTMANE-SICOR   PIC X(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** AREA PARA LOG EMPTO N.FOUND**'.
      *----------------------------------------------------------------*

       01  WRK-EMPTO-NAO-ENCON.
           05 FILLER                   PIC X(03)       VALUE 'CES'.
           05 WRK-CCESTA-SAFRA-BACEN   PIC 9(05)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'VAR'.
           05 WRK-CVARDE-RURAL-BACEN   PIC 9(05)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'CON'.
           05 WRK-CCSCIO-BACEN         PIC 9(01)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'ZON'.
           05 WRK-CZONA-RURAL-BACEN    PIC 9(01)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'ATV'.
           05 WRK-CATVDD-RURAL         PIC 9(05)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'FIN'.
           05 WRK-CFNALD-RURAL         PIC 9(04)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'PRO'.
           05 WRK-CEMPTO-RURAL         PIC 9(06)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'MOD'.
           05 WRK-CMODLD-RURAL-BACEN   PIC 9(08)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'AGR'.
           05 WRK-CAGROP-RURAL-BACEN   PIC 9(05)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'INT'.
           05 WRK-CINTGC-RURAL-BACEN   PIC 9(05)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'IRR'.
           05 WRK-CIGCAO-RURAL-BACEN   PIC 9(05)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'CUL'.
           05 WRK-CCULTR-RURAL-BACEN   PIC 9(05)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'CIC'.
           05 WRK-CCICLO-PROD-BACEN    PIC 9(05)       VALUE ZEROS.
           05 FILLER                   PIC X(03)       VALUE 'TIP'.
           05 WRK-CTPO-CULTR-BACEN     PIC 9(02)       VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** AREA WRK-COR0006-1         **'.
      *----------------------------------------------------------------*

       01  WRK-COR0006-1.
      **** 01 <CODMSG>                      [1..1]
           05 WRK-CODMSG                    PIC  X(009) VALUE SPACES.
      **** 02 <NUMCTRLIF>                   [1..1]
           05 WRK-NUMCTRLIF                 PIC  X(020) VALUE SPACES.
      **** 03 <CNPJENTRESPONS>              [1..1]
7C5.10     05 WRK-CNPJ-ENT-RESPONS          PIC  X(014) VALUE SPACES.
      **** 03 <TPPESSOABENFCRIO>            [1..1]
           05 WRK-TP-PSSOA-BENFCRIO         PIC  X(001) VALUE SPACES.
      **** 04 <CPFCNPJBENFCRIO>             [1..1]
7C5.10     05 WRK-CNPJ-CPF-BENFCRIO         PIC  X(014) VALUE SPACES.
      **** 05 <DTINIPESQ>                   [0..1]
           05 WRK-INI-PESQ                  PIC  9(008) VALUE ZEROS.
      **** 05 <DTFIMPESQ>                   [0..1]
           05 WRK-FIM-PESQ                  PIC  9(008) VALUE ZEROS.
BR4.12**** 05 <NUMREFBCCOR>                 [0..1]
BR4.12     05 WRK-NUM-REF-BACEN             PIC  X(011) VALUE SPACES.
      **** 06 <INDRINCLRECNCTRLD>           [0..1]
           05 WRK-INDR-INCL-REC-NCTRL       PIC  X(001) VALUE SPACES.
      **** 06 <INDRPESQADCLLSTCOOPERD>
           05 WRK-INDR-PESQ-COOPERD         PIC  X(001) VALUE SPACES.
SIC413**** 06 <INDRLSTCOOPERD>              [0..1]
SIC413     05 WRK-INDR-LST-COOPERD          PIC  X(001) VALUE SPACES.
SIC413**** 06 <NUMPAGN>                     [0..1]
SIC413     05 WRK-NUM-PAGN                  PIC  X(003) VALUE SPACES.
BR0521**** 06 <INDRAUTCBENFCRIO>            [0..1]
BR0521     05 WRK-INDR-AUTC-BENFCRIO        PIC  X(001) VALUE SPACES.
      **** 06 <DTMOVTO>                     [0..1]
           05 WRK-DTMOVTO                   PIC  9(008) VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** COPY CRURW6R1              **'.
      *----------------------------------------------------------------*

           COPY 'CRURW6R1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** COPY I#FRWKGE              **'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO      PIC X(526)          VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** COPY I#FRWKLI              **'.
      *----------------------------------------------------------------*

       01 WRK-COPY-LIVRE.
          COPY 'I#FRWKLI'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '** COPY I#FRWKDB              **'.
      *----------------------------------------------------------------*

       01  WRK-COPY-DB2.
           COPY 'I#FRWKDB'.
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)     VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB002
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB016
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0SZ
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB014
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0T4
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0WD
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0SL
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0SO
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0B9
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0TG
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0SJ
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0SV
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0UZ
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0ST
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0SM
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0SN
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0T2
           END-EXEC.
      *
           EXEC SQL
               INCLUDE CRURB0WE
           END-EXEC.
      *

      * ***  CURSOR MENSAGENS RECEBIDAS
           EXEC SQL
            DECLARE CSR01-CRURB0WD CURSOR FOR
            SELECT WCONTD_MSGEM_RURAL
            FROM   DB2PRD.TMNTRC_CONTR_RURAL
            WHERE  CRQUIS_FINCR_BACEN  = :CRURB0WD.CRQUIS-FINCR-BACEN
           END-EXEC.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*** FIM DA WORKING CRUR3C6I ***'.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01 DFHCOMMAREA.
          COPY CRURW00W.
          COPY CRURWE12.
      *
      *================================================================*
       PROCEDURE DIVISION              USING DFHCOMMAREA.
      *================================================================*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR CONTER AS PRINCIPAIS SECTIONS.          *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
           PERFORM 3000-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELA INICIALIZACAO DO PROGRAMA.             *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE FRWKGERR-REGISTRO
                      FRWKGHEA-REGISTRO
                      FRWKGLIV-REGISTRO
                      CRURW00W-BLOCO-RETORNO
      *
           MOVE ZEROS                  TO CRURW00W-COD-RETORNO
           MOVE ZEROS                  TO CRURW00W-COD-ERRO
      ***  OPERACAO REALIZADA COM SUCESSO
           MOVE 'CRUR0320'             TO CRURW00W-COD-MENSAGEM
      *
           PERFORM 1100-CONSISTIR-ENTRADA
           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELA CONSISTENCIA DOS DADOS DE ENTRADA.     *
      *----------------------------------------------------------------*
       1100-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*
      *
           IF  (CRURWE12-NMNTRC-MSGEM       NOT NUMERIC )
           OR  (CRURWE12-NMNTRC-MSGEM       EQUAL ZEROS )
               MOVE 08                 TO CRURW00W-COD-RETORNO
               MOVE '0010'             TO CRURW00W-COD-ERRO
      ***  DADOS INCONSISTENTE
               MOVE 'CRUR0316'         TO CRURW00W-COD-MENSAGEM
               STRING 'NUMERO DE MONITORACAO NAO NUMERICO OU IGUAL '
                      'A ZEROS'
                   DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
               PERFORM 3000-FINALIZAR
           END-IF
      *
           IF  (CRURWE12-NRO-CTRL-IF   EQUAL SPACES OR LOW-VALUES )
               MOVE 08                 TO CRURW00W-COD-RETORNO
               MOVE '0010'             TO CRURW00W-COD-ERRO
      ***  DADOS INCONSISTENTE
               MOVE 'CRUR0316'         TO CRURW00W-COD-MENSAGEM
               MOVE 'NUMERO DE CONTROLE NAO INFORMADO'
                                       TO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
               PERFORM 3000-FINALIZAR
           END-IF.
      *
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELO PROCESSAMENTO DO PROGRAMA.             *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *
           MOVE ZEROS                  TO WRK-NRETOR-BACEN-RESP

           MOVE CRURWE12-NRO-CTRL-IF   TO CRQUIS-FINCR-BACEN
                                       OF CRURB0WD.
           PERFORM 2301-ABRIR-CSR01-CRURB0WD.

           PERFORM 2302-FETCH-CSR01-CRURB0WD.

           IF  WRK-FIM-CRURB0WD        NOT EQUAL 'S'
               PERFORM 2010-OBTER-CPF-CNPJ-COR0006
           END-IF.

           PERFORM 2001-PROCESSA-CSR01-CRURB0WD
             UNTIL WRK-FIM-CRURB0WD    EQUAL 'S'.

           PERFORM 2304-CLOSE-CSR01-CRURB0WD.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       2001-PROCESSA-CSR01-CRURB0WD    SECTION.
      *---------------------------------------------------------------*

           MOVE 1                         TO WRK-POS.
           MOVE WCONTD-MSGEM-RURAL-TEXT   TO COR6R1-COR0006R1-1
                          (WRK-POS:LENGTH OF COR6R1-COR0006R1-1)
           ADD  LENGTH OF COR6R1-COR0006R1-1
                                          TO WRK-POS.

           IF COR6R1-NUM-REF-BCCOR EQUAL SPACES
              PERFORM 2302-FETCH-CSR01-CRURB0WD
              GO TO 2001-99-FIM
           END-IF.

           PERFORM 2200-INSERT-CRURB0WE
           VARYING WRK-POS             FROM WRK-POS
           BY LENGTH OF COR6R1-GRUPO-COR0006R1-OP
           UNTIL WRK-POS               GREATER 32500
           OR WCONTD-MSGEM-RURAL-TEXT  (WRK-POS:6)
                                       NOT EQUAL '{1000}'.

           PERFORM 2302-FETCH-CSR01-CRURB0WD.
           .
      *----------------------------------------------------------------*
       2001-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       2010-OBTER-CPF-CNPJ-COR0006     SECTION.
      *---------------------------------------------------------------*

           MOVE WCONTD-MSGEM-RURAL-TEXT   OF CRURB0WD
                TO WRK-COR0006-1(1:LENGTH OF WRK-COR0006-1).

           IF  WRK-TP-PSSOA-BENFCRIO      EQUAL 'F'
               MOVE WRK-CNPJ-CPF-BENFCRIO TO WRK-CPF-BACEN
               IF  WRK-CPF                NOT NUMERIC
               OR  WRK-CPF                EQUAL ZEROS
               OR  WRK-CONTROLE-CPF       NOT NUMERIC
                   STRING 'CPF INVALIDO. COR0006R1: '
                          WRK-COR0006-1
                   DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
                   PERFORM 9300-ERRO-LIVRE
                   MOVE 08                TO CRURW00W-COD-RETORNO
                   MOVE '0010'            TO CRURW00W-COD-ERRO
      ***  DADOS INCONSISTENTE
                   MOVE 'CRUR0316'        TO CRURW00W-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
               ELSE
                   MOVE WRK-CPF           TO WRK-CPF-CNPJ-CRURB0WE
                   MOVE ZEROS             TO WRK-FILIAL-CRURB0WE
                   MOVE WRK-CONTROLE-CPF  TO WRK-CTRL-CRURB0WE
               END-IF
           ELSE
7C5.10         MOVE WRK-CNPJ-CPF-BENFCRIO TO WRK-CNPJ-BACEN
               IF  WRK-CNPJ               NOT NUMERIC
               OR  WRK-CNPJ               EQUAL ZEROS
               OR  WRK-FILIAL-CNPJ        NOT NUMERIC
               OR  WRK-CONTROLE-CNPJ      NOT NUMERIC
                   STRING 'CNPJ INVALIDO. COR0006R1: '
                          WRK-COR0006-1
                   DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
                   PERFORM 9300-ERRO-LIVRE
                   MOVE 08                TO CRURW00W-COD-RETORNO
                   MOVE '0010'            TO CRURW00W-COD-ERRO
      ***  DADOS INCONSISTENTE
                   MOVE 'CRUR0316'        TO CRURW00W-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
               ELSE
                   MOVE '0'               TO WRK-CPF-CNPJ-CRURB0WE(1:1)
                   MOVE WRK-CNPJ          TO WRK-CPF-CNPJ-CRURB0WE(2:8)
                   MOVE WRK-FILIAL-CNPJ   TO WRK-FILIAL-CRURB0WE
                   MOVE WRK-CONTROLE-CNPJ TO WRK-CTRL-CRURB0WE
               END-IF
           END-IF.

           MOVE WRK-CPF-CNPJ-CRURB0WE-N   TO CCPF-CNPJ-DSTNA
                                          OF CRURB0WE.
           MOVE WRK-FILIAL-CRURB0WE-N     TO CFLIAL-CNPJ-DSTNA
                                          OF CRURB0WE.
           MOVE WRK-CTRL-CRURB0WE-N       TO CCTRL-CPF-DSTNA
                                          OF CRURB0WE.

      *----------------------------------------------------------------*
       2010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       2200-INSERT-CRURB0WE            SECTION.
      *---------------------------------------------------------------*

           MOVE WCONTD-MSGEM-RURAL-TEXT
                (WRK-POS : LENGTH      OF COR6R1-GRUPO-COR0006R1-OP)
                                       TO COR6R1-GRUPO-COR0006R1-OP.

           MOVE CRURWE12-NMNTRC-MSGEM  TO NMNTRC-MSGEM-RURAL OF CRURB0WE

           EXEC SQL
             SELECT MAX(NRETOR_BACEN_RESP)+1
             INTO :CRURB0WE.NRETOR-BACEN-RESP
             FROM DB2PRD.TRESP_PSSOA_BACEN
             WHERE NMNTRC_MSGEM_RURAL = :CRURB0WE.NMNTRC-MSGEM-RURAL
           END-EXEC

           IF  SQLCODE                 EQUAL -305
               MOVE 1                  TO WRK-NRETOR-BACEN-RESP
           ELSE
               MOVE NRETOR-BACEN-RESP  OF CRURB0WE
                                       TO WRK-NRETOR-BACEN-RESP
           END-IF

           MOVE CRURWE12-NMNTRC-MSGEM  TO NMNTRC-MSGEM-RURAL OF CRURB0WE

           MOVE WRK-NRETOR-BACEN-RESP  TO NRETOR-BACEN-RESP  OF CRURB0WE
           MOVE COR6R1-NUM-REF-BCCOR   TO CREFT-BACEN-CREDT  OF CRURB0WE

           IF  COR6R1-COD-PROG-LINHA-CRED EQUAL ALL '*'
               IF  COR6R1-COD-PROG-LINHA-CRED-COO
                                       EQUAL ALL '*'
                   MOVE 0999           TO CLIN-SIST-BACEN    OF CRURB0TG
               ELSE
ID6416             EVALUATE COR6R1-COD-PROG-LINHA-CRED-COO
   .                  WHEN '0001'
   .                  WHEN '0050'
   .                  WHEN '0070'
   .                  WHEN '0999'
   .                     MOVE COR6R1-COD-PROG-LINHA-CRED-COO
   .                                     TO WRK-CLIN-X
   .                     MOVE WRK-CLIN-9 TO CLIN-SIST-BACEN
   .                                     OF CRURB0TG
   .                  WHEN OTHER
   .                     GO TO 2200-99-FIM
ID6416             END-EVALUATE
               END-IF
           ELSE
ID6416         EVALUATE COR6R1-COD-PROG-LINHA-CRED
   .              WHEN '0001'
   .              WHEN '0050'
   .              WHEN '0070'
   .              WHEN '0999'
   .                 MOVE COR6R1-COD-PROG-LINHA-CRED
   .                                 TO WRK-CLIN-X
   .                 MOVE WRK-CLIN-9 TO CLIN-SIST-BACEN
   .                                 OF CRURB0TG
   .              WHEN OTHER
   .                 GO TO 2200-99-FIM
ID6416         END-EVALUATE
           END-IF.

           EXEC SQL
             SELECT CLIN_RURAL_BACEN
             INTO   :CRURB0TG.CLIN-RURAL-BACEN
             FROM   DB2PRD.TLIN_CREDT_BACEN
             WHERE  CLIN_SIST_BACEN    = :CRURB0TG.CLIN-SIST-BACEN
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2200'             TO FRWKGDB2-LOCAL
               MOVE 'TLIN_CREDT_BACEN' TO FRWKGDB2-NOME-TABELA
               MOVE '2200-INSERT-CRURB0WE'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CLIN-SIST-BACEN    OF CRURB0TG
                                       TO WRK-ZD8
               STRING 'LINHA BACEN NAO ENCONTRADA:'
                      COR6R1-COD-PROG-LINHA-CRED
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
TESTE ******* GGOTTO*2200-99-FIM
           END-IF.

           IF SQLCODE                  EQUAL +100
ID6416        PERFORM 2210-INSERIR-CRURB0TG
ID6416        IF WRK-INCLUSAO-OK       EQUAL 'S'
ID6416*      MMOVE 1                  TTO CLIN-RURAL-BACEN
ID6416           MOVE WRK-CLIN-9       TO CLIN-RURAL-BACEN
                                       OF CRURB0WE
                 MOVE '*'              TO CUSUAR-MANUT OF CRURB0WE(2:1)
                 MOVE ZEROS            TO WRK-CUSUAR-MANUT-NULL
ID6416        END-IF
           ELSE
              MOVE CLIN-RURAL-BACEN    OF CRURB0TG
                                       TO CLIN-RURAL-BACEN
                                       OF CRURB0WE
           END-IF.

           PERFORM 2220-ENQUADRAR-EMPTO-BACEN

           IF  CEMPTO-RURAL-BACEN        OF CRURB0T2 EQUAL ZEROS
           OR  CITEM-PROD-BACEN          OF CRURB0T2 EQUAL ZEROS
               MOVE 1                    TO CEMPTO-RURAL-BACEN
                                         OF CRURB0WE
               MOVE 1                    TO CITEM-PROD-BACEN
                                         OF CRURB0WE
               MOVE '*'                  TO CUSUAR-MANUT
                                         OF CRURB0WE(1:1)
               MOVE ZEROS                TO WRK-CUSUAR-MANUT-NULL
               STRING 'REGISTRO SEM CODIGO BACEN'
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           ELSE
               MOVE CEMPTO-RURAL-BACEN   OF CRURB0T2
                                         TO CEMPTO-RURAL-BACEN
                                         OF CRURB0WE
               MOVE CITEM-PROD-BACEN     OF CRURB0T2
                                         TO CITEM-PROD-BACEN
                                         OF CRURB0WE
           END-IF.

BR0521     IF  COR6R1-CNPJ-ENT-RESP-OR IS NUMERIC     AND
.              COR6R1-CNPJ-ENT-RESP-OR GREATER ZEROS
.              MOVE COR6R1-CNPJ-ENT-RESP-OR
.                                      TO WRK-CNPJ-ENT-RESP-OR
.              MOVE WRK-CNPJ-N         TO CCNPJ-ENTID-RESP   OF CRURB0WE
.              MOVE WRK-FILIAL-N       TO CFLIAL-CNPJ-ENTID  OF CRURB0WE
.              MOVE WRK-CTRL-N         TO CCTRL-CNPJ-ENTID   OF CRURB0WE
.          ELSE
.              MOVE -1                 TO WRK-CCNPJ-ENTID-RESP-NULL
.              MOVE -1                 TO WRK-CFLIAL-CNPJ-ENTID-NULL
.              MOVE -1                 TO WRK-CCTRL-CNPJ-ENTID-NULL
.          END-IF.
.
.          IF  COR6R1-CNPJ-PART        IS NUMERIC     AND
.              COR6R1-CNPJ-PART        GREATER ZEROS
.              MOVE COR6R1-CNPJ-PART
.                                      TO WRK-CNPJ-ENT-RESP-OR
.              MOVE WRK-CNPJ-N         TO CCNPJ-PARTC        OF CRURB0WE
.              MOVE WRK-FILIAL-N       TO CFLIAL-CNPJ-PARTC  OF CRURB0WE
.              MOVE WRK-CTRL-N         TO CCTRL-CNPJ-PARTC   OF CRURB0WE
.          ELSE
.              MOVE -1                 TO WRK-CCNPJ-PARTC-NULL
.              MOVE -1                 TO WRK-CFLIAL-CNPJ-PARTC-NULL
.              MOVE -1                 TO WRK-CCTRL-CNPJ-PARTC-NULL
BR0521     END-IF.

           MOVE COR6R1-DT-EMS          TO WRK-DATA-AMD
           MOVE CORR WRK-DATA-AMD      TO WRK-DATA-DB2
           MOVE WRK-DATA-DB2           TO DGERAC-DOCTO-CONTR OF CRURB0WE

           MOVE COR6R1-DT-VENC         TO WRK-DATA-AMD
           MOVE CORR WRK-DATA-AMD      TO WRK-DATA-DB2
           MOVE WRK-DATA-DB2           TO DVCTO-CONTR        OF CRURB0WE

           IF  COR6R1-TP-FNTE-REC      NUMERIC
               MOVE ZEROS              TO WRK-ZD8
               MOVE COR6R1-TP-FNTE-REC TO WRK-CH4
               MOVE WRK-ZD8            TO CREC-FINCR-FINAN   OF CRURB0WE
           END-IF.

           IF  COR6R1-COD-MOD-SEGR     IS NUMERIC
               MOVE COR6R1-COD-MOD-SEGR TO CTPO-SEGUR-CONTT  OF CRURB0WE
           END-IF.

           IF  COR6R1-COD-MUNIC        IS NUMERIC
               MOVE COR6R1-COD-MUNIC   TO CMUN         OF CRURB0WE
           END-IF.

           IF  COR6R1-VLR-PARCL-CRED       IS NUMERIC
               MOVE COR6R1-VLR-PARCL-CRED  TO WRK-VALOR
               MOVE WRK-VALOR-DB2          TO VFINAN-EMPTO-CREDT
                                                             OF CRURB0WE
           ELSE
               MOVE ZEROS                  TO VFINAN-EMPTO-CREDT
                                                             OF CRURB0WE
           END-IF.

           IF  COR6R1-VLR-PARCL-REC-PROPRIO
                                           IS NUMERIC
               MOVE COR6R1-VLR-PARCL-REC-PROPRIO
                                           TO WRK-VALOR
               MOVE WRK-VALOR-DB2          TO VPCELA-REC-PPRIO
                                                             OF CRURB0WE
           ELSE
               MOVE ZEROS                  TO VPCELA-REC-PPRIO
                                                             OF CRURB0WE
           END-IF.

           IF  COR6R1-VLR-PARCL-MANUT-FAMLR
                                           IS NUMERIC
               MOVE COR6R1-VLR-PARCL-MANUT-FAMLR
                                           TO WRK-VALOR
               MOVE WRK-VALOR-DB2          TO VPCELA-MANUT-FAMLR
                                                             OF CRURB0WE
           ELSE
               MOVE ZEROS                  TO VPCELA-MANUT-FAMLR
                                                             OF CRURB0WE
           END-IF.

           IF  COR6R1-VLR-PARCL-COOPERD    IS NUMERIC
               MOVE COR6R1-VLR-PARCL-COOPERD
                                           TO WRK-VALOR
               MOVE WRK-VALOR-DB2          TO VPCELA-COOP-RURAL
                                                             OF CRURB0WE
           ELSE
               MOVE ZEROS                  TO VPCELA-COOP-RURAL
                                                             OF CRURB0WE
           END-IF.

4.11       MOVE COR6R1-TP-PESSOA-COOPERD  TO CTPO-PSSOA-COOP OF CRURB0WE
4.11
4.11       IF  COR6R1-CNPJ-CPF-COOPERD     NOT NUMERIC
4.11           MOVE ZEROS                 TO WRK-ZEROS
4.11                                         WRK-CPF-CNPJ-14
4.11       ELSE
4.11           MOVE ZEROS                 TO WRK-ZEROS
4.11           MOVE COR6R1-CNPJ-CPF-COOPERD
4.11                                      TO WRK-CPF-CNPJ-14
4.11       END-IF
4.11       MOVE WRK-CPF-CNPJ-15           TO CDOCTO-PSSOA-COOP
                                                             OF CRURB0WE
4.11
4.11       MOVE COR6R1-TP-PESSOA-BENFCRIO TO CTPO-PSSOA-BNEFC
4.11                                                         OF CRURB0WE
4.11
4.11       IF  COR6R1-CNPJ-CPF-BENFCRIO    NOT NUMERIC
4.11           MOVE ZEROS                 TO WRK-ZEROS
4.11                                         WRK-CPF-CNPJ-14
4.11       ELSE
4.11           MOVE ZEROS                 TO WRK-ZEROS
4.11           MOVE COR6R1-CNPJ-CPF-BENFCRIO
4.11                                      TO WRK-CPF-CNPJ-14
4.11       END-IF
4.11       MOVE WRK-CPF-CNPJ-15           TO CDOCTO-PSSOA-BNEFC
                                                             OF CRURB0WE
4.11       IF  CTPO-PSSOA-COOP OF CRURB0WE    NOT EQUAL 'F' AND 'J'
4.11           MOVE -1                    TO WRK-CTPO-PSSOA-COOP-NULL
4.11       ELSE
4.11           MOVE ZEROS                 TO WRK-CTPO-PSSOA-COOP-NULL
4.11       END-IF
4.11  *
4.11       IF  CDOCTO-PSSOA-COOP OF CRURB0WE  EQUAL ZEROS
4.11           MOVE -1                    TO WRK-CDOCTO-PSSOA-COOP-NULL
4.11       ELSE
4.11           MOVE ZEROS                 TO WRK-CDOCTO-PSSOA-COOP-NULL
4.11       END-IF
4.11  *
4.11       IF  CTPO-PSSOA-BNEFC OF CRURB0WE   NOT EQUAL 'F' AND 'J'
4.11           MOVE -1                    TO WRK-CTPO-PSSOA-BNEFC-NULL
4.11       ELSE
4.11           MOVE ZEROS                 TO WRK-CTPO-PSSOA-BNEFC-NULL
4.11       END-IF
4.11  *
4.11       IF  CDOCTO-PSSOA-BNEFC OF CRURB0WE EQUAL ZEROS
4.11           MOVE -1                    TO WRK-CDOCTO-PSSOA-BNEFC-NULL
4.11       ELSE
4.11           MOVE ZEROS                 TO WRK-CDOCTO-PSSOA-BNEFC-NULL
4.11       END-IF

           MOVE WRK-CPF-CNPJ-CRURB0WE-N   TO CCPF-CNPJ-DSTNA
                                          OF CRURB0WE.
           MOVE WRK-FILIAL-CRURB0WE-N     TO CFLIAL-CNPJ-DSTNA
                                          OF CRURB0WE.
           MOVE WRK-CTRL-CRURB0WE-N       TO CCTRL-CPF-DSTNA
                                          OF CRURB0WE.

           MOVE 'CRUR3C6I'                TO CUSUAR-INCL
                                          OF CRURB0WE.
BR4.12     IF COR6R1-COD-SUB-PROG       EQUAL ALL '*'
BR4.12        MOVE SPACES               TO CSUB-PROG-RURAL  OF CRURB0WE
BR4.12        MOVE -1                   TO WRK-CSUB-PROG-RURAL-NULL
BR4.12     ELSE
BR4.12        MOVE ZEROS                TO WRK-CSUB-PROG-RURAL-NULL
BR4.12        MOVE COR6R1-COD-SUB-PROG  TO CSUB-PROG-RURAL  OF CRURB0WE
BR4.12     END-IF.
5.0        IF COR6R1-CNPJ-BASE IS NUMERIC
.              STRING COR6R1-CNPJ-BASE
.              DELIMITED BY SIZE INTO WRK-DECM-8
               MOVE WRK-DECM-8 TO CCNPJ-BASE-MUTUA
.              MOVE ZEROS TO WRK-CCNPJ-BASE-MUTU-NULL
.          ELSE
.              MOVE ZEROS TO CCNPJ-BASE-MUTUA
.              MOVE -1 TO WRK-CCNPJ-BASE-MUTU-NULL
.          END-IF
.          IF COR6R1-NREF-BCOR IS NUMERIC
.            STRING COR6R1-NREF-BCOR
.            DELIMITED BY SIZE INTO WRK-DECM-11
             MOVE WRK-DECM-11 TO NREFT-BACEN-INVES
.            MOVE ZEROS TO WRK-NREFT-BACEN-INV-NULL
.          ELSE
.            MOVE ZEROS TO NREFT-BACEN-INVES
.            MOVE -1 TO WRK-NREFT-BACEN-INV-NULL
.          END-IF
.
.          IF  COR6R1-VL-PCELA-INVST
.                                          IS NUMERIC
.              MOVE COR6R1-VL-PCELA-INVST
.                                          TO WRK-VALOR
.              MOVE WRK-VALOR-DB2          TO VPCELA-INVES-RURAL
.                                                            OF CRURB0WE
               MOVE ZEROS TO WRK-VPCELA-INVES-RU-NULL
.          ELSE
.              MOVE ZEROS                  TO VPCELA-INVES-RURAL
.                                                            OF CRURB0WE
.              MOVE -1 TO WRK-VPCELA-INVES-RU-NULL
.          END-IF
.          MOVE -1 TO WRK-CFLIAL-BASE-MUT-NULL
5.0        MOVE -1 TO WRK-CCTRL-BASE-MUTU-NULL
           EXEC SQL
           INSERT INTO DB2PRD.TRESP_PSSOA_BACEN
           ( NMNTRC_MSGEM_RURAL
           , NRETOR_BACEN_RESP
           , CREFT_BACEN_CREDT
           , DGERAC_DOCTO_CONTR
           , DVCTO_CONTR
           , CREC_FINCR_FINAN
           , CEMPTO_RURAL_BACEN
           , CITEM_PROD_BACEN
           , CTPO_SEGUR_CONTT
           , CMUN
           , VFINAN_EMPTO_CREDT
           , VPCELA_REC_PPRIO
           , VPCELA_MANUT_FAMLR
           , VPCELA_COOP_RURAL
           , CLIN_RURAL_BACEN
           , CCPF_CNPJ_DSTNA
           , CFLIAL_CNPJ_DSTNA
           , CCTRL_CPF_DSTNA
           , CUSUAR_INCL
           , HINCL_REG
           , CUSUAR_MANUT
           , CTPO_PSSOA_COOP
           , CDOCTO_PSSOA_COOP
           , CTPO_PSSOA_BNEFC
           , CDOCTO_PSSOA_BNEFC
BR4.12*******CSUB_PROG_RURAL)
5.0        ,CCNPJ_BASE_MUTUA
.          ,CFLIAL_BASE_MUTUA
.          ,CCTRL_BASE_MUTUA
.          ,NREFT_BACEN_INVES
5.0        ,VPCELA_INVES_RURAL
BR0521     ,CCNPJ_ENTID_RESP
BR0521     ,CFLIAL_CNPJ_ENTID
BR0521     ,CCTRL_CNPJ_ENTID
BR0521     ,CCNPJ_PARTC
BR0521     ,CFLIAL_CNPJ_PARTC
BR0521     ,CCTRL_CNPJ_PARTC)
           VALUES
           ( :CRURB0WE.NMNTRC-MSGEM-RURAL
           , :CRURB0WE.NRETOR-BACEN-RESP
           , :CRURB0WE.CREFT-BACEN-CREDT
           , :CRURB0WE.DGERAC-DOCTO-CONTR
           , :CRURB0WE.DVCTO-CONTR
           , :CRURB0WE.CREC-FINCR-FINAN
           , :CRURB0WE.CEMPTO-RURAL-BACEN
           , :CRURB0WE.CITEM-PROD-BACEN
           , :CRURB0WE.CTPO-SEGUR-CONTT
           , :CRURB0WE.CMUN
           , :CRURB0WE.VFINAN-EMPTO-CREDT
           , :CRURB0WE.VPCELA-REC-PPRIO
           , :CRURB0WE.VPCELA-MANUT-FAMLR
           , :CRURB0WE.VPCELA-COOP-RURAL
           , :CRURB0WE.CLIN-RURAL-BACEN
           , :CRURB0WE.CCPF-CNPJ-DSTNA
           , :CRURB0WE.CFLIAL-CNPJ-DSTNA
           , :CRURB0WE.CCTRL-CPF-DSTNA
           , 'CRUR3C6I'
           ,  CURRENT TIMESTAMP
           , :CRURB0WE.CUSUAR-MANUT
             :WRK-CUSUAR-MANUT-NULL
4.11       , :CRURB0WE.CTPO-PSSOA-COOP
4.11              :WRK-CTPO-PSSOA-COOP-NULL
4.11       , :CRURB0WE.CDOCTO-PSSOA-COOP
4.11              :WRK-CDOCTO-PSSOA-COOP-NULL
4.11       , :CRURB0WE.CTPO-PSSOA-BNEFC
4.11              :WRK-CTPO-PSSOA-BNEFC-NULL
4.11       , :CRURB0WE.CDOCTO-PSSOA-BNEFC
4.11              :WRK-CDOCTO-PSSOA-BNEFC-NULL
BR4.12********CRURB0WE.CSUB-PROG-RURAL
BR4.2************:WRK-CSUB-PROG-RURAL-NULL)
5.0        , :CRURB0WE.CCNPJ-BASE-MUTUA
.                 :WRK-CCNPJ-BASE-MUTU-NULL
.          , :CRURB0WE.CFLIAL-BASE-MUTUA
.                 :WRK-CFLIAL-BASE-MUT-NULL
.          , :CRURB0WE.CCTRL-BASE-MUTUA
.                 :WRK-CCTRL-BASE-MUTU-NULL
.          , :CRURB0WE.NREFT-BACEN-INVES
.                  :WRK-NREFT-BACEN-INV-NULL
.          , :CRURB0WE.VPCELA-INVES-RURAL
5.0                :WRK-VPCELA-INVES-RU-NULL
BR0521     , :CRURB0WE.CCNPJ-ENTID-RESP
.                  :WRK-CCNPJ-ENTID-RESP-NULL
.          , :CRURB0WE.CFLIAL-CNPJ-ENTID
.                  :WRK-CFLIAL-CNPJ-ENTID-NULL
.          , :CRURB0WE.CCTRL-CNPJ-ENTID
.                  :WRK-CCTRL-CNPJ-ENTID-NULL
.          , :CRURB0WE.CCNPJ-PARTC
.                  :WRK-CCNPJ-PARTC-NULL
.          , :CRURB0WE.CFLIAL-CNPJ-PARTC
.                  :WRK-CFLIAL-CNPJ-PARTC-NULL
.          , :CRURB0WE.CCTRL-CNPJ-PARTC
BR0521             :WRK-CCTRL-CNPJ-PARTC-NULL)
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL  'W')
               STRING 'SQLCODE <> ZEROS. SQLERRM: '
                      SQLERRM
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
               SET DB2-INSERT          TO TRUE
               MOVE '2200'             TO FRWKGDB2-LOCAL
               MOVE 'TRESP_PSSOA_BACEN' TO FRWKGDB2-NOME-TABELA
               MOVE '2200-INSERT-CRURB0WE'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

ID6416*---------------------------------------------------------------*
   .   2210-INSERIR-CRURB0TG            SECTION.
   .  *---------------------------------------------------------------*
   .
   .       MOVE WRK-CLIN-9             TO CLIN-RURAL-BACEN   OF CRURB0TG
   .                                      CLIN-SIST-BACEN    OF CRURB0TG
   .
   .       EVALUATE WRK-CLIN-9
   .          WHEN 001
   .            MOVE 'PRONAF - PROGRAMA NACIONAL DE FORTALECIMENTO DA AG
   .  -              'RICULTURA FAMILIAR' TO RLIN-CREDT-BACEN
   .                                      OF CRURB0TG
   .          WHEN 050
   .            MOVE 'PRONAMP - PROGRAMA NACIONAL DE APOIO AO MEDIO PROD
   .  -              'UTOR RURAL '     TO RLIN-CREDT-BACEN  OF CRURB0TG
   .          WHEN 070
   .            MOVE 'FUNCAFE - PROGRAMA DE DEFESA DA ECONOMIA CAFEEIRA'
   .                                   TO RLIN-CREDT-BACEN  OF CRURB0TG
   .          WHEN 999
   .            MOVE 'FINANCIAMENTO SEM VÍNCULO A PROGRAMA ESPECÍFICO'
   .                                  TO RLIN-CREDT-BACEN   OF CRURB0TG
   .       END-EVALUATE
   .
   .       MOVE '0'                   TO CSGL-LIN-CREDT
   .                                                        OF CRURB0TG.
   .       MOVE ZEROS                 TO CFINAN-SIST-BACEN
   .                                                        OF CRURB0TG.
   .       MOVE '31.12.2099'          TO DFIM-VGCIA-REG
   .                                                        OF CRURB0TG.
   .       MOVE 'A'                   TO CSIT-REG-RURAL
   .                                                        OF CRURB0TG.
   .       MOVE 'CRUR3C6I'            TO CUSUAR-INCL
   .                                                        OF CRURB0TG.
   .       MOVE SPACES                TO CUSUAR-MANUT       OF CRURB0TG.
   .       MOVE -1                    TO WRK-CUSUAR-MANUT-N.
   .       MOVE SPACES                TO HMANUT-REG         OF CRURB0TG.
   .       MOVE -1                    TO WRK-HMANUT-REG-N.
   .
   .       EXEC SQL
   .       INSERT INTO DB2PRD.TLIN_CREDT_BACEN
   .       ( CLIN_RURAL_BACEN
   .       , CLIN_SIST_BACEN
   .       , RLIN_CREDT_BACEN
   .       , CSGL_LIN_CREDT
   .       , CFINAN_SIST_BACEN
   .       , DINIC_VGCIA_REG
   .       , DFIM_VGCIA_REG
   .       , CSIT_REG_RURAL
   .       , CUSUAR_INCL
   .       , HINCL_REG
   .       , CUSUAR_MANUT
   .       , HMANUT_REG)
   .       VALUES
   .       ( :CRURB0TG.CLIN-RURAL-BACEN
   .       , :CRURB0TG.CLIN-SIST-BACEN
   .       , :CRURB0TG.RLIN-CREDT-BACEN
   .       , :CRURB0TG.CSGL-LIN-CREDT
   .       , :CRURB0TG.CFINAN-SIST-BACEN
   .       ,  CURRENT DATE
   .       , :CRURB0TG.DFIM-VGCIA-REG
   .       , :CRURB0TG.CSIT-REG-RURAL
   .       , :CRURB0TG.CUSUAR-INCL
   .       ,  CURRENT_TIMESTAMP
   .       , :CRURB0TG.CUSUAR-MANUT
   .         :WRK-CUSUAR-MANUT-N
   .       , :CRURB0TG.HMANUT-REG
   .         :WRK-HMANUT-REG-N)
   .       END-EXEC.
   .
   .       IF  (SQLCODE                NOT EQUAL ZEROS )
   .       OR  (SQLWARN0               EQUAL  'W')
   .           STRING 'SQLCODE <> ZEROS. SQLERRM: '
   .                  SQLERRM
   .           DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
   .           PERFORM 9300-ERRO-LIVRE
   .           SET DB2-INSERT          TO TRUE
   .           MOVE '2210'             TO FRWKGDB2-LOCAL
   .           MOVE 'TLIN_CREDT_BACEN' TO FRWKGDB2-NOME-TABELA
   .           MOVE '2210-INSERIR-CRURB0TG'
   .                                   TO FRWKGHEA-IDEN-PARAGRAFO
   .           PERFORM 9000-ERRO-DB2
   .       END-IF.
   .
   .       IF SQLCODE  EQUAL ZEROS
   .          MOVE 'S' TO WRK-INCLUSAO-OK
   .       END-IF.
   .
   .  *---------------------------------------------------------------*
   .   2210-99-FIM.                    EXIT.
ID6416*---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-ENQUADRAR-EMPTO-BACEN      SECTION.
      *----------------------------------------------------------------*
      *
           MOVE COR6R1-COD-ENPNMNT     TO WRK-EMPREEND-SICOR.

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-ATIV-SICOR         TO WRK-CH1.
           MOVE WRK-ZD1                TO CATVDD-RURAL-BACEN
                                       OF CRURB002
           PERFORM 2221-ACESSAR-CRURB002

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-FINAL-SICOR        TO WRK-CH1.
           MOVE WRK-ZD1                TO CFNALD-RURAL-BACEN
                                       OF CRURB016
           PERFORM 2222-ACESSAR-CRURB016

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-MODALID-SICOR      TO WRK-CH2.
           MOVE WRK-CH5                TO CMODLD-SIST-BACEN
                                       OF CRURB0SZ.
           PERFORM 2223-ACESSAR-CRURB0SZ

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-PRODUTO-SICOR      TO WRK-CH4.
           MOVE WRK-ZD4                TO CPRODT-RURAL-BACEN
                                       OF CRURB014.
           PERFORM 2224-ACESSAR-CRURB014

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-VARIED-SICOR       TO WRK-CH3.
           MOVE WRK-ZD3                TO CVARDE-SIST-BACEN
                                       OF CRURB0T4.
           PERFORM 2225-ACESSAR-CRURB0T4

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-CONSORC-SICOR      TO WRK-CH1.
           MOVE WRK-ZD1                TO CCSCIO-BACEN
                                       OF CRURB0SO.

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-CESTA-SICOR        TO WRK-CH1.
           MOVE WRK-ZD1                TO CCESTA-SIST-BACEN
                                       OF CRURB0SL.
           PERFORM 2226-ACESSAR-CRURB0SL

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-ZONEAM-SICOR       TO WRK-CH1.
           MOVE WRK-ZD1                TO CZONA-RURAL-BACEN
                                       OF CRURB0SO.

BR1217     MOVE COR6R1-COD-SIST-PRODC  TO WRK-SIST-PROD-SICOR

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-TPAGROP-SICOR      TO WRK-CH1
           MOVE WRK-ZD1                TO CAGROP-SIST-BACEN
                                       OF CRURB0SJ.

           PERFORM 2230-ACESSAR-CRURB0SJ

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-TPINTEG-SICOR      TO WRK-CH1
           MOVE WRK-ZD1                TO CINTGC-SIST-BACEN
                                       OF CRURB0SV.

           PERFORM 2231-ACESSAR-CRURB0SV

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-GRAOSEM-SICOR      TO WRK-CH1
           MOVE WRK-ZD1                TO CTPO-CULTR-BACEN
                                       OF CRURB0UZ.

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-TPIRRIG-SICOR      TO WRK-CH2
           MOVE WRK-ZD2                TO CIGCAO-SIST-BACEN
                                       OF CRURB0ST.

           PERFORM 2233-ACESSAR-CRURB0ST

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-TPCULTV-SICOR      TO WRK-CH2
           MOVE WRK-ZD2                TO CCULTR-SIST-BACEN
                                       OF CRURB0SN.

           PERFORM 2234-ACESSAR-CRURB0SN

           MOVE ZEROS                  TO WRK-ZD8.
           MOVE WRK-FASECIC-SICOR      TO WRK-CH2
           MOVE WRK-ZD2                TO CCICLO-SIST-BACEN
                                       OF CRURB0SM.

           PERFORM 2235-ACESSAR-CRURB0SM

           PERFORM 2228-ACESSAR-CRURB0B9.

      *---------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2221-ACESSAR-CRURB002           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO CATVDD-RURAL OF CRURB002.

           EXEC SQL
             SELECT CATVDD_RURAL
             INTO   :CRURB002.CATVDD-RURAL
             FROM   DB2PRD.TATVDD_RURAL
             WHERE  CATVDD_RURAL_BACEN = :CRURB002.CATVDD-RURAL-BACEN
             AND    DINIC_ATVDD_RURAL  <= CURRENT DATE
             AND    DFIM_ATVDD_RURAL   >= CURRENT DATE
             AND    CSIT_REG_RURAL     <> 'I'
             ORDER  BY CSIT_REG_RURAL
                    ,  DINIC_ATVDD_RURAL  DESC
                    ,  HMANUT_REG         DESC
             FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2221'             TO FRWKGDB2-LOCAL
               MOVE 'TATVDD_RURAL'     TO FRWKGDB2-NOME-TABELA
               MOVE '2221-ACESSAR-CRURB002'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CATVDD-RURAL-BACEN OF CRURB002
                                       TO WRK-ZD8
               STRING 'ATIVIDADE NAO ENCONTRADA:'
                      WRK-CH8
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2221-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2222-ACESSAR-CRURB016           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO CFNALD-RURAL OF CRURB016.

           EXEC SQL
             SELECT CFNALD_RURAL
             INTO   :CRURB016.CFNALD-RURAL
             FROM   DB2PRD.TFNALD_RURAL
             WHERE  CFNALD_RURAL_BACEN = :CRURB016.CFNALD-RURAL-BACEN
             AND    DINIC_VGCIA_RURAL  <= CURRENT DATE
             AND    DFIM_VGCIA_RURAL   >= CURRENT DATE
             AND    CSIT_REG_RURAL     <> 'I'
             ORDER  BY CSIT_REG_RURAL
                    ,  DINIC_VGCIA_RURAL DESC
                    ,  HMANUT_REG        DESC
             FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2222'             TO FRWKGDB2-LOCAL
               MOVE 'TFNALD_RURAL'     TO FRWKGDB2-NOME-TABELA
               MOVE '2222-ACESSAR-CRURB016'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CFNALD-RURAL-BACEN OF CRURB016
                                       TO WRK-ZD8
               STRING 'FINALIDADE NAO ENCONTRADA:'
                      WRK-CH8
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2222-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2223-ACESSAR-CRURB0SZ           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS               TO CMODLD-RURAL-BACEN OF CRURB0SZ.

           EXEC SQL
             SELECT CMODLD_RURAL_BACEN
             INTO   :CRURB0SZ.CMODLD-RURAL-BACEN
             FROM   DB2PRD.TMODLD_RURAL_BACEN
             WHERE  CMODLD_SIST_BACEN  = :CRURB0SZ.CMODLD-SIST-BACEN
             AND    DINIC_VGCIA_REG    <= CURRENT DATE
             AND    DFIM_VGCIA_REG     >= CURRENT DATE
             AND    CSIT_REG_RURAL     <> 'I'
             ORDER  BY CSIT_REG_RURAL
                    ,  DINIC_VGCIA_REG DESC
                    ,  HMANUT_REG      DESC
             FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2223'             TO FRWKGDB2-LOCAL
               MOVE 'TMODLD_RURAL_BACEN'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2223-ACESSAR-CRURB0SZ'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CMODLD-SIST-BACEN  OF CRURB0SZ
                                       TO WRK-ZD8
               STRING 'MODALIDADE NAO ENCONTRADA:'
                      WRK-CH8
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2223-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2224-ACESSAR-CRURB014           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO CEMPTO-RURAL OF CRURB014.

           EXEC SQL
             SELECT CEMPTO_RURAL
             INTO   :CRURB014.CEMPTO-RURAL
             FROM   DB2PRD.TEMPTO_RURAL
             WHERE  CPRODT_RURAL_BACEN = :CRURB014.CPRODT-RURAL-BACEN
             AND    DINIC_VGCIA_EMPTO  <= CURRENT DATE
             AND    DFIM_VGCIA_EMPTO   >= CURRENT DATE
             AND    CSIT_REG_RURAL     <> 'I'
             ORDER  BY CSIT_REG_RURAL
                    ,  DINIC_VGCIA_EMPTO DESC
                    ,  HMANUT_REG        DESC
             FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2224'             TO FRWKGDB2-LOCAL
               MOVE 'TEMPTO_RURAL'     TO FRWKGDB2-NOME-TABELA
               MOVE '2224-ACESSAR-CRURB014'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CPRODT-RURAL-BACEN OF CRURB014
                                       TO WRK-ZD8
               STRING 'PRODUTO NAO ENCONTRADO:'
                      WRK-CH8
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2224-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2225-ACESSAR-CRURB0T4           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS               TO CVARDE-RURAL-BACEN OF CRURB0T4.

           EXEC SQL
             SELECT CVARDE_RURAL_BACEN
             INTO   :CRURB0T4.CVARDE-RURAL-BACEN
             FROM   DB2PRD.TVARDE_PRODT_BACEN
             WHERE  CVARDE_SIST_BACEN  = :CRURB0T4.CVARDE-SIST-BACEN
             AND    DINIC_VGCIA_REG    <= CURRENT DATE
             AND    DFIM_VGCIA_REG     >= CURRENT DATE
             AND    CSIT_REG_RURAL     <> 'I'
             ORDER  BY CSIT_REG_RURAL
                    ,  DINIC_VGCIA_REG   DESC
                    ,  HMANUT_REG        DESC
             FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2225'             TO FRWKGDB2-LOCAL
               MOVE 'TVARDE_PRODT_BACEN'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2225-ACESSAR-CRURB0T4'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CVARDE-SIST-BACEN  OF CRURB0T4
                                       TO WRK-ZD8
               STRING 'VARIEDADE NAO ENCONTRADA:'
                      WRK-CH8
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2225-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2226-ACESSAR-CRURB0SL           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS               TO CCESTA-SAFRA-BACEN OF CRURB0SL.

           EXEC SQL
             SELECT CCESTA_SAFRA_BACEN
             INTO   :CRURB0SL.CCESTA-SAFRA-BACEN
             FROM   DB2PRD.TCESTA_SIST_BACEN
             WHERE  CCESTA_SIST_BACEN  = :CRURB0SL.CCESTA-SIST-BACEN
             AND    DINIC_VGCIA_REG    <= CURRENT DATE
             AND    DFIM_VGCIA_REG     >= CURRENT DATE
             AND    CSIT_REG_RURAL     <> 'I'
             ORDER  BY CSIT_REG_RURAL
                    ,  DINIC_VGCIA_REG   DESC
                    ,  HMANUT_REG        DESC
             FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2226'             TO FRWKGDB2-LOCAL
               MOVE 'TCESTA_SIST_BACEN'   TO FRWKGDB2-NOME-TABELA
               MOVE '2226-ACESSAR-CRURB0SL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CCESTA-SIST-BACEN  OF CRURB0SL
                                       TO WRK-ZD8
               STRING 'CESTA DE SAFRAS NAO ENCONTRADA:'
                      WRK-CH8
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2226-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2228-ACESSAR-CRURB0B9           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO CEMPTO-RURAL-BACEN OF CRURB0T2
                                          CITEM-PROD-BACEN   OF CRURB0T2

           MOVE CATVDD-RURAL           OF CRURB002
                                       TO CATVDD-RURAL OF CRURB0B9.
           MOVE CFNALD-RURAL           OF CRURB016
                                       TO CFNALD-RURAL OF CRURB0B9.
           MOVE CMODLD-RURAL-BACEN     OF CRURB0SZ
                                       TO CMODLD-RURAL-BACEN
                                                       OF CRURB0B9.
           MOVE CEMPTO-RURAL           OF CRURB014
                                       TO CEMPTO-RURAL OF CRURB0B9.
           MOVE CCESTA-SAFRA-BACEN     OF CRURB0SL
                                       TO CCESTA-SAFRA-BACEN
                                                       OF CRURB0SO.
           MOVE CVARDE-RURAL-BACEN     OF CRURB0T4
                                       TO CVARDE-RURAL-BACEN
                                                       OF CRURB0SO.

           EXEC SQL
           SELECT SIS.CEMPTO_RURAL_BACEN
           ,      SIS.CITEM_PROD_BACEN
           INTO   :CRURB0T2.CEMPTO-RURAL-BACEN
           ,      :CRURB0T2.CITEM-PROD-BACEN
           FROM   DB2PRD.TEMPTO_SIST_BACEN  EMP
           ,      DB2PRD.TMODLD_RURAL       MOD
           ,      DB2PRD.TSIST_PROD_BACEN   SIS
           ,      DB2PRD.TVARDE_PRODT_BACEN VAR
           WHERE  EMP.CCESTA_SAFRA_BACEN = :CRURB0SO.CCESTA-SAFRA-BACEN
           AND    EMP.CCSCIO_BACEN       = :CRURB0SO.CCSCIO-BACEN
           AND    EMP.CZONA_RURAL_BACEN  = :CRURB0SO.CZONA-RURAL-BACEN
           AND    EMP.DINIC_VGCIA_REG    <= CURRENT DATE
           AND    EMP.DFIM_VGCIA_REG     >= CURRENT DATE
           AND    EMP.CSIT_REG_RURAL     <> 'I'
           AND    MOD.CATVDD_RURAL       = :CRURB0B9.CATVDD-RURAL
           AND    MOD.CFNALD_RURAL       = :CRURB0B9.CFNALD-RURAL
           AND    MOD.CEMPTO_RURAL       = :CRURB0B9.CEMPTO-RURAL
           AND    MOD.CMODLD_RURAL_BACEN = :CRURB0B9.CMODLD-RURAL-BACEN
           AND    MOD.DINIC_VGCIA_MODLD  <= CURRENT DATE
           AND    MOD.DFIM_VGCIA_MODLD   >= CURRENT DATE
           AND    MOD.CSIT_REG_RURAL     <> 'I'
           AND    SIS.CAGROP_RURAL_BACEN = :CRURB0SJ.CAGROP-RURAL-BACEN
           AND    SIS.CINTGC_RURAL_BACEN = :CRURB0SV.CINTGC-RURAL-BACEN
           AND    SIS.CIGCAO_RURAL_BACEN = :CRURB0ST.CIGCAO-RURAL-BACEN
           AND    SIS.CCULTR_RURAL_BACEN = :CRURB0SN.CCULTR-RURAL-BACEN
           AND    SIS.CCICLO_PROD_BACEN  = :CRURB0SM.CCICLO-PROD-BACEN
           AND    SIS.CTPO_CULTR_BACEN   = :CRURB0UZ.CTPO-CULTR-BACEN
           AND    VAR.CVARDE_SIST_BACEN  = :CRURB0T4.CVARDE-SIST-BACEN
           AND    VAR.DINIC_VGCIA_REG    <= CURRENT DATE
           AND    VAR.DFIM_VGCIA_REG     >= CURRENT DATE
           AND    VAR.CSIT_REG_RURAL     <> 'I'
           AND    EMP.CMODLD_RURAL       = MOD.CMODLD_RURAL
           AND    SIS.CEMPTO_RURAL_BACEN = EMP.CEMPTO_RURAL_BACEN
           AND    VAR.CVARDE_RURAL_BACEN = EMP.CVARDE_RURAL_BACEN
           ORDER  BY VAR.CVARDE_RURAL_BACEN
           FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2228'             TO FRWKGDB2-LOCAL
               MOVE 'TMODLD_RURAL'     TO FRWKGDB2-NOME-TABELA
               MOVE '2228-ACESSAR-CRURB0B9'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CCESTA-SAFRA-BACEN OF CRURB0SO
                                       TO WRK-CCESTA-SAFRA-BACEN
               MOVE CVARDE-RURAL-BACEN OF CRURB0SO
                                       TO WRK-CVARDE-RURAL-BACEN
               MOVE CCSCIO-BACEN       OF CRURB0SO
                                       TO WRK-CCSCIO-BACEN
               MOVE CZONA-RURAL-BACEN  OF CRURB0SO
                                       TO WRK-CZONA-RURAL-BACEN
               MOVE CATVDD-RURAL       OF CRURB0B9
                                       TO WRK-CATVDD-RURAL
               MOVE CFNALD-RURAL       OF CRURB0B9
                                       TO WRK-CFNALD-RURAL
               MOVE CEMPTO-RURAL       OF CRURB0B9
                                       TO WRK-CEMPTO-RURAL
               MOVE CMODLD-RURAL-BACEN OF CRURB0B9
                                       TO WRK-CMODLD-RURAL-BACEN
               MOVE CAGROP-RURAL-BACEN OF CRURB0SJ
                                       TO WRK-CAGROP-RURAL-BACEN
               MOVE CINTGC-RURAL-BACEN OF CRURB0SV
                                       TO WRK-CINTGC-RURAL-BACEN
               MOVE CIGCAO-RURAL-BACEN OF CRURB0ST
                                       TO WRK-CIGCAO-RURAL-BACEN
               MOVE CCULTR-RURAL-BACEN OF CRURB0SN
                                       TO WRK-CCULTR-RURAL-BACEN
               MOVE CCICLO-PROD-BACEN  OF CRURB0SM
                                       TO WRK-CCICLO-PROD-BACEN
               MOVE CTPO-CULTR-BACEN   OF CRURB0UZ
                                       TO WRK-CTPO-CULTR-BACEN
               MOVE WRK-EMPTO-NAO-ENCON
                                       TO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2228-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2230-ACESSAR-CRURB0SJ           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS               TO CAGROP-RURAL-BACEN OF CRURB0SJ.

           EXEC SQL
             SELECT CAGROP_RURAL_BACEN
             INTO   :CRURB0SJ.CAGROP-RURAL-BACEN
             FROM   DB2PRD.TAGROP_SIST_BACEN
             WHERE  CAGROP_SIST_BACEN  =  :CRURB0SJ.CAGROP-SIST-BACEN
             AND    DINIC_VGCIA_REG    <=  CURRENT DATE
             AND    DFIM_VGCIA_REG     >=  CURRENT DATE
             AND    CSIT_REG_RURAL     <>  'I'
             AND    CAGROP_RURAL_BACEN >= :CRURB0SJ.CAGROP-RURAL-BACEN
             ORDER  BY CSIT_REG_RURAL
                    ,  DINIC_VGCIA_REG   DESC
                    ,  HMANUT_REG        DESC
             FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2230'             TO FRWKGDB2-LOCAL
               MOVE 'TAGROP_SIST_BACEN'   TO FRWKGDB2-NOME-TABELA
               MOVE '2230-ACESSAR-CRURB0SJ'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CAGROP-SIST-BACEN  OF CRURB0SJ
                                       TO WRK-ZD8
               STRING 'CODIGO AGROPECUARIO NAO ENCONTRADO:'
                      WRK-CH8
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2231-ACESSAR-CRURB0SV           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS               TO CINTGC-RURAL-BACEN OF CRURB0SV.

           EXEC SQL
             SELECT CINTGC_RURAL_BACEN
             INTO   :CRURB0SV.CINTGC-RURAL-BACEN
             FROM   DB2PRD.TINTGC_RURAL_BACEN
             WHERE  CINTGC_SIST_BACEN  =  :CRURB0SV.CINTGC-SIST-BACEN
             AND    DINIC_VGCIA_REG    <=  CURRENT DATE
             AND    DFIM_VGCIA_REG     >=  CURRENT DATE
             AND    CSIT_REG_RURAL     <>  'I'
             AND    CINTGC_RURAL_BACEN >= :CRURB0SV.CINTGC-RURAL-BACEN
             ORDER  BY CSIT_REG_RURAL
                    ,  DINIC_VGCIA_REG   DESC
                    ,  HMANUT_REG        DESC
             FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2231'             TO FRWKGDB2-LOCAL
               MOVE 'TINTGC_RURAL_BACEN'  TO FRWKGDB2-NOME-TABELA
               MOVE '2231-ACESSAR-CRURB0SV'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CINTGC-SIST-BACEN  OF CRURB0SV
                                       TO WRK-ZD8
               STRING 'CODIGO DE INTEGRACAO NAO ENCONTRADO:'
                      WRK-CH8
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2231-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2233-ACESSAR-CRURB0ST           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS               TO CIGCAO-RURAL-BACEN OF CRURB0ST.

           EXEC SQL
             SELECT CIGCAO_RURAL_BACEN
             INTO   :CRURB0ST.CIGCAO-RURAL-BACEN
             FROM   DB2PRD.TIGCAO_SIST_BACEN
             WHERE  CIGCAO_SIST_BACEN  =  :CRURB0ST.CIGCAO-SIST-BACEN
             AND    DINIC_VGCIA_REG    <=  CURRENT DATE
             AND    DFIM_VGCIA_REG     >=  CURRENT DATE
             AND    CSIT_REG_RURAL     <>  'I'
             AND    CIGCAO_RURAL_BACEN >= :CRURB0ST.CIGCAO-RURAL-BACEN
             ORDER  BY CSIT_REG_RURAL
                    ,  DINIC_VGCIA_REG   DESC
                    ,  HMANUT_REG        DESC
             FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2233'             TO FRWKGDB2-LOCAL
               MOVE 'TIGCAO_SIST_BACEN'   TO FRWKGDB2-NOME-TABELA
               MOVE '2233-ACESSAR-CRURB0ST'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CIGCAO-SIST-BACEN  OF CRURB0ST
                                       TO WRK-ZD8
               STRING 'SISTEMA DE IRRIGACAO NAO ENCONTRADO:'
                      WRK-CH8
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2233-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2234-ACESSAR-CRURB0SN           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS               TO CCULTR-RURAL-BACEN OF CRURB0SN.

           EXEC SQL
             SELECT CCULTR_RURAL_BACEN
             INTO   :CRURB0SN.CCULTR-RURAL-BACEN
             FROM   DB2PRD.TCULTR_SIST_BACEN
             WHERE  CCULTR_SIST_BACEN  =  :CRURB0SN.CCULTR-SIST-BACEN
             AND    DINIC_VGCIA_REG    <=  CURRENT DATE
             AND    DFIM_VGCIA_REG     >=  CURRENT DATE
             AND    CSIT_REG_RURAL     <>  'I'
             AND    CCULTR_RURAL_BACEN >= :CRURB0SN.CCULTR-RURAL-BACEN
             ORDER  BY CSIT_REG_RURAL
                    ,  DINIC_VGCIA_REG   DESC
                    ,  HMANUT_REG        DESC
             FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2234'             TO FRWKGDB2-LOCAL
               MOVE 'TCULTR_SIST_BACEN'   TO FRWKGDB2-NOME-TABELA
               MOVE '2234-ACESSAR-CRURB0SN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CCULTR-SIST-BACEN  OF CRURB0SN
                                       TO WRK-ZD8
               STRING 'SISTEMA DE CULTIVO NAO ENCONTRADO:'
                      WRK-CH8
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2234-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2235-ACESSAR-CRURB0SM           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS               TO CCICLO-PROD-BACEN  OF CRURB0SM.

           EXEC SQL
             SELECT CCICLO_PROD_BACEN
             INTO   :CRURB0SM.CCICLO-PROD-BACEN
             FROM   DB2PRD.TCICLO_PROD_BACEN
             WHERE  CCICLO_SIST_BACEN  =  :CRURB0SM.CCICLO-SIST-BACEN
             AND    DINIC_VGCIA_REG    <=  CURRENT DATE
             AND    DFIM_VGCIA_REG     >=  CURRENT DATE
             AND    CSIT_REG_RURAL     <>  'I'
             AND    CCICLO_PROD_BACEN  >= :CRURB0SM.CCICLO-PROD-BACEN
             ORDER  BY CSIT_REG_RURAL
                    ,  DINIC_VGCIA_REG   DESC
                    ,  HMANUT_REG        DESC
             FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2235'             TO FRWKGDB2-LOCAL
               MOVE 'TCICLO_PROD_BACEN'   TO FRWKGDB2-NOME-TABELA
               MOVE '2235-ACESSAR-CRURB0SM'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE CCICLO-SIST-BACEN  OF CRURB0SM
                                       TO WRK-ZD8
               STRING 'CODIGO DO CICLO NAO ENCONTRADO:'
                      WRK-CH8
               DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               PERFORM 9300-ERRO-LIVRE
           END-IF.

      *---------------------------------------------------------------*
       2235-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2301-ABRIR-CSR01-CRURB0WD       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                OPEN CSR01-CRURB0WD
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-OPEN            TO TRUE
               MOVE '2301'             TO FRWKGDB2-LOCAL
               MOVE 'TMNTRC_CONTR_RURAL'  TO FRWKGDB2-NOME-TABELA
               MOVE '2301-ABRIR-CSR01-CRURB0WD'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

      *---------------------------------------------------------------*
       2301-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2302-FETCH-CSR01-CRURB0WD       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                FETCH CSR01-CRURB0WD
                INTO :CRURB0WD.WCONTD-MSGEM-RURAL
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL  'W')
               SET DB2-FETCH           TO TRUE
               MOVE '2302'             TO FRWKGDB2-LOCAL
               MOVE 'TMNTRC_CONTR_RURAL'  TO FRWKGDB2-NOME-TABELA
               MOVE '2302-FETCH-CSR01-CRURB0WD'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CRURB0WD
           END-IF.

      *---------------------------------------------------------------*
       2302-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2304-CLOSE-CSR01-CRURB0WD       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-CRURB0WD
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '2304'             TO FRWKGDB2-LOCAL
               MOVE 'TMNTRC_CONTR_RURAL'  TO FRWKGDB2-NOME-TABELA
               MOVE '2304-CLOSE-CSR01-CRURB0WD'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

      *---------------------------------------------------------------*
       2304-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS
               RETURN
           END-EXEC
           .
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO DB2.                         *
      *----------------------------------------------------------------*
       9000-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-DB2               TO TRUE
           MOVE 12                     TO CRURW00W-COD-RETORNO
           MOVE '0180'                 TO CRURW00W-COD-ERRO
      ***  ERRO DE ACESSO AS BASES DE DADOS
           MOVE 'CRUR0042'             TO CRURW00W-COD-MENSAGEM
      ***  <#001,450#>
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE
           MOVE SQLCA                  TO FRWKGDB2-SQLCA
           MOVE SQLCODE                TO FRWKGDB2-SQLCODE
           MOVE WRK-COPY-DB2           TO WRK-BLOCO-INFO-ERRO
      *
           PERFORM 9999-API-ERROS
           .
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DE ERRO LIVRE                           *
      *----------------------------------------------------------------*
       9300-ERRO-LIVRE                 SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-LIVRE             TO TRUE

           MOVE FRWKGLIV-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE 'CRUR1421'             TO FRWKGLIV-COD-MENSAGEM
           MOVE 1                      TO FRWKGLIV-IDIOMA
           MOVE 'CRUR3C6I'             TO FRWKGHEA-NOME-PROGRAMA

           IF  FRWKGHEA-IDEN-PARAGRAFO EQUAL SPACES OR LOW-VALUES
               MOVE 'CRUR3C6I'         TO FRWKGHEA-IDEN-PARAGRAFO
           END-IF.

           MOVE WRK-COPY-LIVRE
                             (1:LENGTH OF WRK-COPY-LIVRE)
                                       TO WRK-BLOCO-INFO-ERRO
           .
           EXEC CICS LINK
               PROGRAM  (WRK-FRWK1999)
               COMMAREA (WRK-AREA-ERRO)
               LENGTH          (LENGTH OF WRK-AREA-ERRO)
               NOHANDLE
           END-EXEC

           IF (EIBRESP NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF
           .
      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DE ERRO CICS.                           *
      *----------------------------------------------------------------*
      * ROTINA PARA CHAMADA DA API DE ERROS                            *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
               PROGRAM (WRK-FRWK1999)
               COMMAREA (WRK-AREA-ERRO)
               LENGTH (LENGTH OF WRK-AREA-ERRO)
               NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF
           PERFORM 3000-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
