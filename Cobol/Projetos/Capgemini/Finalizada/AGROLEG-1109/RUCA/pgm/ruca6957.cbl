      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUCA6957.
       AUTHOR. WAGNER N.
      *================================================================*
      *                   S I S   C O N S U L T O R I A                *
      *----------------------------------------------------------------*
      *                                                                *
      *  PROGRAMA    : RUCA6957                                        *
      *  ANALISTA    : WAGNER N.                                       *
      *  DATA        : SETEMBRO / 2020                                 *
      *  OBJETIVO    : MANUTENCAO DAS PARCELAS DE LIBERACAO DE RECURSOS*
      *                                                                *
      *----------------------------------------------------------------*
      *  TELAS:                                                        *
      *                                                                *
      *     RUCAT957 - LIBERACAO DE RECURSOS                           *
      *                                                                *
      *----------------------------------------------------------------*
      *  NAVEGACAO:                                                    *
      *                                                                *
      *  RETORNA:                                                      *
      *     RUCAT950 - MENU DE AJUSTE SICOR                            *
      *     RUCAT951 - DADOS DO INSTRUMENTO DE CREDITO                 *
      *     RUCAT956 - PRODUTO CONSORCIADO E LOCALIZACAO EMPREENDIMENTO*
      *                                                                *
      *----------------------------------------------------------------*
      *  MODULOS CHAMADOS:                                             *
      *                                                                *
      *     RUCA8808 - INCLUI AS ALTERACOES NAS BASES DE MANUTENCAO    *
      *     RUCA8810 - FORMATA MENSAGEM COR0001                        *
      *     RUCA8812 - ENVIAR COR0001 E COR0005 PARA RCOR              *
      *     RUCA8814 - FORMATA MENSAGEM COR0005                        *
      *     BRAD0660 - OTIMIZACAO DOS DADOS                            *
      *     BRAD1205 - CONSISTIR DATA                                  *
      *     POOL5005 - COMUNICACAO                                     *
      *     BRAD7100 - TRATAR ERROS IMS/DB2                            *
      *     BRAD7600 - DATA E HORA                                     *
      *                                                                *
      *----------------------------------------------------------------*
      *  BASES BD2:                                                    *
      *                                                                *
      *     RUCAB004 - DB2PRD.RUCA_CAD_DEP                             *
      *     RUCAB014 - DB2PRD.TLOG_MANUT_CONTR                         *
      *     RUCAB015 - DB2PRD.TMANUT_CONTR_RECOR                       *
      *     RUECB048 - DB2PRD.TAPOIO_UTILZ_MDULO                       *
      *     RUFIB002 - DB2PRD.CONTRATO_RURAL                           *
      *                                                                *
      *================================================================*
DPS01 *----------------------------------------------------------------*
.     *               A L T E R A C A O                                *
.     *----------------------------------------------------------------*
.     *    ANALISTA ...:  DESIREE PESSOA     - BRADESCO / GP.39        *
.     *    DATA........:  10/10/2022                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:                                               *
.     *      PALIATIVO PARA CANCELAR ENVIO DO CONTRATO 9173321 QUE     *
.     *      FICOU PRESSO NO TMNF                                      *
DPS01 *================================================================*
DPS02 *----------------------------------------------------------------*
.     *               A L T E R A C A O                                *
.     *----------------------------------------------------------------*
.     *    ANALISTA ...:  DESIREE PESSOA     - BRADESCO / GP.39        *
.     *    DATA........:  10/10/2022                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:                                               *
.     *      PALIATIVO PARA CANCELAR ENVIO DO CONTRATO 9171490 QUE     *
.     *      FICOU PRESSO NO TMNF                                      *
DPS02 *================================================================*
DPS03 *----------------------------------------------------------------*
.     *               A L T E R A C A O                                *
.     *----------------------------------------------------------------*
.     *    ANALISTA ...:  DESIREE PESSOA     - BRADESCO / GP.39        *
.     *    DATA........:  10/10/2022                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:                                               *
.     *      PALIATIVO PARA CANCELAR ENVIO DOS CONTRATOS 8718866 E
.     *      9171937 QUE FICARAM PRESOS NO TMNF                        *
DPS03 *================================================================*
PRI005*                ALTERACAO - PRIME                               *
PRI005*----------------------------------------------------------------*
PRI005*                                                                *
PRI005* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI005* DATA...............: 31/01/2023                                *
PRI005* OBJETIVOS..........: MANUTENCAO DE CONTRATO LIQUIDADO          *
PRI005*================================================================*
DPS04 *----------------------------------------------------------------*
.     *               A L T E R A C A O                                *
.     *----------------------------------------------------------------*
.     *    ANALISTA ...:  DESIREE PESSOA     - BRADESCO / GP.39        *
.     *    DATA........:  10/10/2022                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:                                               *
.     *      PALIATIVO PARA CANCELAR ENVIO DO CONTRATO 9182530         *
.     *      QUE FICARAM PRESOS NO TMNF                                *
DPS04 *================================================================*
DPS05 *----------------------------------------------------------------*
.     *               A L T E R A C A O                                *
.     *----------------------------------------------------------------*
.     *    ANALISTA ...:  DESIREE PESSOA     - BRADESCO / GP.39        *
.     *    DATA........:  25/07/2023                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:                                               *
.     *      PALIATIVO PARA CANCELAR ENVIO DOS CONTRATOS 9180267 E     *
.     *      9182530 QUE FICARAM PRESOS NO TMNF                        *
DPS05 *================================================================*
DPS06 *----------------------------------------------------------------*
.     *               A L T E R A C A O                                *
.     *----------------------------------------------------------------*
.     *    ANALISTA ...:  DESIREE PESSOA     - BRADESCO / GP.39        *
.     *    DATA........:  11/12/2023                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:                                               *
.     *      PALIATIVO PARA CANCELAR ENVIO DO CONTRATO 9196382         *
.     *      QUE FICOU PRESO NO TMNF                                   *
DPS06 *================================================================*
DPS07 *----------------------------------------------------------------*
.     *               A L T E R A C A O                                *
.     *----------------------------------------------------------------*
.     *    ANALISTA ...:  DESIREE PESSOA     - BRADESCO / GP.39        *
.     *    DATA........:  11/12/2023                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:                                               *
.     *      PALIATIVO PARA CANCELAR ENVIO DO CONTRATO 9186203         *
.     *      QUE FICOU PRESO NO TMNF.                                  *
DPS07 *================================================================*
DPS08 *----------------------------------------------------------------*
.     *               A L T E R A C A O                                *
.     *----------------------------------------------------------------*
.     *    ANALISTA ...:  DESIREE PESSOA     - BRADESCO / GP.39        *
.     *    DATA........:  16/05/2024                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:                                               *
.     *      PALIATIVO PARA CANCELAR ENVIO DO CONTRATO 9196567         *
.     *      QUE FICOU PRESO NO TMNF                                   *
DPS08 *================================================================*
DPS09 *----------------------------------------------------------------*
.     *               A L T E R A C A O                                *
.     *----------------------------------------------------------------*
.     *    ANALISTA ...:  DESIREE PESSOA     - BRADESCO / GP.39        *
.     *    DATA........:  05/08/2024                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:                                               *
.     *      PALIATIVO PARA CANCELAR ENVIO DOS CONTRATOS 9192362,      *
.     *      9194205, 9194240, 9196825 E 9196928 QUE FICARAM PRESOS    *
.     *      NO TMNF.                                                  *
DPS09 *================================================================*
DPS10 *----------------------------------------------------------------*
.     *               A L T E R A C A O                                *
.     *----------------------------------------------------------------*
.     *    ANALISTA ...:  DESIREE PESSOA     - BRADESCO / GP.39        *
.     *    DATA........:  09/08/2024                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:                                               *
.     *      PALIATIVO PARA CANCELAR ENVIO DOS CONTRATOS QUE FICARAM   *
.     *      PRESOS NO TMNF.                                           *
DPS10 *================================================================*
DPS11 *----------------------------------------------------------------*
.     *               A L T E R A C A O                                *
.     *----------------------------------------------------------------*
.     *    ANALISTA ...:  DESIREE PESSOA     - BRADESCO / GP.39        *
.     *    DATA........:  09/08/2024                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:                                               *
.     *      PALIATIVO PARA CANCELAR ENVIO DO CONTRATO 9192362         *
.     *      QUE FICOU PRESO NO TMNF.                                  *
DPS11 *================================================================*


      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '***  INICIO DA WORKING RUCA6957  ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS  PARA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-GU                      PIC  X(04)         VALUE 'GU'.
       01  WRK-CHNG                    PIC  X(04)         VALUE 'CHNG'.
       01  WRK-ISRT                    PIC  X(04)         VALUE 'ISRT'.
       01  WRK-VERSAO                  PIC  X(06)         VALUE
                                                              'VRS002'.
PRI005 01  WRK-RUCA8516                PIC  X(08)         VALUE
PRI005                                                      'RUCA8516'.
       01  WRK-FUNCAO                  PIC  X(04)         VALUE SPACES.
       01  WRK-TELA                    PIC  X(08)         VALUE SPACES.

       01  WRK-COD-USER                PIC  X(07)         VALUE SPACES.
       01  WRK-COD-USER-N              REDEFINES
           WRK-COD-USER                PIC  9(07).

       01  WRK-COD-DEPTO.
           05  FILLER                  PIC  X(02)         VALUE SPACES.
           05  WRK-COD-DEPTO-N         PIC  9(04)         VALUE ZEROS.

       01  WRK-MENSAGEM.
           05  FILLER                  PIC  X(04)         VALUE SPACES.
           05  WRK-TRANSACAO           PIC  X(08)         VALUE SPACES.
           05  FILLER                  PIC  X(1988)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREA DE ATRIBUTO  ***'.
      *----------------------------------------------------------------*

      * 1 | NUMERICO | NORMAL   | DESPROTEGIDO |                       *
       01  WRK-209                     PIC S9(08) COMP    VALUE +209.

      * 2 | NUMERICO | NORMAL   | DESPROTEGIDO | POSICIONAMENTO CURSOR *
       01  WRK-49361                   PIC S9(08) COMP    VALUE +49361.

      * 3 | NUMERICO | BRILHANTE| DESPROTEGIDO |                       *
       01  WRK-217                     PIC S9(08) COMP    VALUE +217.

      *3-9| NUMERICO | BRILHANTE| DESPROTEGIDO | POSICIONAMENTO CURSOR *
       01  WRK-49369                   PIC S9(08) COMP    VALUE +49369.

      * 4 | NUMERICO | ALFA     | PROTEGIDO    |                       *
       01  WRK-225                     PIC S9(08) COMP    VALUE +225.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS DE MENSAGENS  ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG01                   PIC  X(79)         VALUE
           'ENTRE COM OS DADOS E TECLE <ENTER> OU TECLE <PF11> PARA ENVI
      -    'AR'.

       01  WRK-MSG02                   PIC  X(79)         VALUE
           'PF INVALIDA'.

       01  WRK-MSG03                   PIC  X(79)         VALUE
           'CAMPO COM CONTEUDO INVALIDO'.

       01  WRK-MSG04                   PIC  X(79)         VALUE
           'OBRIGATORIO DIGITAR A VIRGULA DECIMAL'.

       01  WRK-MSG06                   PIC  X(79)         VALUE
           'OBRIGATORIO INFORMAR DATA E VALOR'.

       01  WRK-MSG07                   PIC  X(79)         VALUE
           'DATA INFORMADA DEVE SER UTIL E IGUAL OU MAIOR QUE A DATA ATU
      -    'AL'.

       01  WRK-MSG08                   PIC  X(79)         VALUE
           'DATA DE LIBERACAO DEVE SER MENOR/IGUAL A DATA DE PAGAMENTO D
      -    'A PRIMEIRA PARCELA'.

       01  WRK-MSG09                   PIC  X(79)         VALUE
           'PF INVALIDA. SALVE AS ALTERACOES PARA PODER AVANCAR'.

       01  WRK-MSG10                   PIC  X(79)         VALUE
           'TECLE <PF6> PARA CONFIRMAR A ALTERACAO OU <ENTER> PARA ALTER
      -    'AR'.

       01  WRK-MSG11                   PIC  X(79)         VALUE
           'ALTERACAO SALVA. TECLE <PF11> PARA ENVIAR A SOLICITACAO AO B
      -    'ACEN'.

       01  WRK-MSG12                   PIC  X(79)         VALUE
           'TECLE <ENTER> P/VERIFICAR O STATUS, <PF11> PARA REENVIAR OU
      -    '<PF9> PARA CANCELAR'.

       01  WRK-MSG13                   PIC  X(79)         VALUE
           'EM ALTERACAO'.

       01  WRK-MSG14                   PIC  X(79)         VALUE
           'AGUARDANDO O ENVIO PARA O BACEN'.

       01  WRK-MSG15                   PIC  X(79)         VALUE
           'ENVIADO PARA O BACEN'.

       01  WRK-MSG16                   PIC  X(79)         VALUE
           'AGUARDANDO O RETORNO DO BACEN'.

       01  WRK-MSG17                   PIC  X(79)         VALUE
           'MANUTENCAO REJEITADA PELO BACEN'.

       01  WRK-MSG18                   PIC  X(79)         VALUE
           'MANUTENCAO EFETIVADA'.

       01  WRK-MSG19                   PIC  X(79)         VALUE
           'MANUTENCAO FOI CANCELADA'.

       01  WRK-MSG20                   PIC  X(79)         VALUE
           'CANCELAMENTO EFETUADO COM SUCESSO'.

       01  WRK-MSG21                   PIC  X(79)         VALUE
           'PF INVALIDA. MANUTENCAO AGUARDANDO RETORNO DO BACEN'.

       01  WRK-MSG22                   PIC  X(79)         VALUE
           'PF INVALIDA. MANUTENCAO FOI REJEITADA PELO BACEN'.

       01  WRK-MSG23                   PIC  X(79)         VALUE
           'PF INVALIDA. MANUTENCAO JA FOI EFETIVADA'.

       01  WRK-MSG24                   PIC  X(79)         VALUE
           'PF INVALIDA. MANUTENCAO JA FOI CANCELADA'.

       01  WRK-MSG25                   PIC  X(79)         VALUE
           'ENVIO EFETUADO COM SUCESSO, AGUARDE RETORNO'.

       01  WRK-MSG26                   PIC  X(79)         VALUE
           'VALOR DAS PARCELAS NAO PODE SER MAIOR QUE O VALOR FINANCIADO
      -    ''.

       01  WRK-MSG27                   PIC  X(79)         VALUE
           'NAO EXISTEM ALTERACOES PARA SEREM ENVIADAS'.

PRI005 01  WRK-MSG28                   PIC  X(79)         VALUE
PRI005     'CONTRATO LIQUIDADO, TECLE <PF9> PARA CANCELAR OU <PF11> PARA
PRI005-    ' ENVIAR'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS PARA DRESSCODE  ***'.
      *----------------------------------------------------------------*

       01  WRK-S09-DRESS               PIC +9(09)         VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S09-DRESS.
           05  FILLER                  PIC  X(02).
           05  WRK-X08-DRESS           PIC  X(08).
       01  FILLER                      REDEFINES  WRK-S09-DRESS.
           05  FILLER                  PIC  X(07).
           05  WRK-N03-DRESS           PIC  9(03).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES E INDEXADORES ***'.
      *----------------------------------------------------------------*

       01  IND-LIN                     PIC  9(05) COMP-3  VALUE ZEROS.
       01  IND-CHAR                    PIC  9(02) COMP-3  VALUE ZEROS.
       01  IND-TAM                     PIC  9(02) COMP-3  VALUE ZEROS.
       01  IND-POSI                    PIC  9(02) COMP-3  VALUE ZEROS.
       01  IND-MASC                    PIC  9(02) COMP-3  VALUE ZEROS.

       01  ACU-REGISTRO                PIC  9(05) COMP-3  VALUE ZEROS.
       01  ACU-VALOR-AUX               PIC S9(13)V99
                                                  COMP-3  VALUE ZEROS.
       01  ACU-VALOR-TOTAL             PIC S9(13)V99
                                                  COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS AUXILIARES  ***'.
      *----------------------------------------------------------------*

       01  WRK-CSEQ-CTRL-MDULO         PIC S9(05) COMP-3  VALUE ZEROS.
       01  WRK-DTVCTO-AUX              PIC  X(08)         VALUE SPACES.
       01  WRK-MODULO                  PIC  X(08)         VALUE SPACES.
       01  WRK-INCONSIS                PIC  X(01)         VALUE SPACES.
       01  WRK-ERRO-DATA               PIC  X(01)         VALUE SPACES.
       01  WRK-VLR-INCONSIS            PIC  X(01)         VALUE SPACES.
       01  WRK-DEC-INCONSIS            PIC  X(01)         VALUE SPACES.
       01  WRK-VALOR-ENT               PIC  X(20)         VALUE SPACES.
       01  WRK-MASCARA-ED              PIC  X(20)         VALUE
           'Z.ZZZ.ZZZ.ZZZ.ZZZ,ZZ'.

       01  WRK-VALOR-SAI               PIC  X(20)         VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-VALOR-SAI.
           05  WRK-N13V99-SAI-ED       PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.

       01  WRK-PRIMEIRA-PARC.
           05  WRK-PARC-PRI            PIC  9(03)         VALUE ZEROS.
           05  WRK-DTVCTO-PRI          PIC  X(08)         VALUE SPACES.

       01  WRK-DATA-ATUAL              PIC  X(08)         VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-DATA-ATUAL.
           05  WRK-ANO-ATUAL           PIC  X(04).
           05  WRK-MES-ATUAL           PIC  X(02).
           05  WRK-DIA-ATUAL           PIC  X(02).

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  X(02)         VALUE SPACES.
           05  WRK-PT1-DB2             PIC  X(01)         VALUE SPACES.
           05  WRK-MES-DB2             PIC  X(02)         VALUE SPACES.
           05  WRK-PT2-DB2             PIC  X(01)         VALUE SPACES.
           05  WRK-ANO-DB2             PIC  X(04)         VALUE SPACES.

       01  WRK-DATA-DTEVENTO           PIC  X(08)         VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-DATA-DTEVENTO.
           05  WRK-ANO-DTEVENTO        PIC  X(04).
           05  WRK-MES-DTEVENTO        PIC  X(02).
           05  WRK-DIA-DTEVENTO        PIC  X(02).

       01  WRK-TIMESTAMP.
           05  WRK-ANO-TIMES           PIC  9(04)          VALUE ZEROS.
           05  WRK-TR1-TIMES           PIC  X(01)          VALUE '-'.
           05  WRK-MES-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-TR2-TIMES           PIC  X(01)          VALUE '-'.
           05  WRK-DIA-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-TR3-TIMES           PIC  X(01)          VALUE '-'.
           05  WRK-HOR-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-PT1-TIMES           PIC  X(01)          VALUE '.'.
           05  WRK-MIN-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-PT2-TIMES           PIC  X(01)          VALUE '.'.
           05  WRK-SEG-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-PT3-TIMES           PIC  X(01)          VALUE '.'.
           05  WRK-MIL-TIMES           PIC  9(06)          VALUE ZEROS.

DPS10  01  WRK-DT-INCL-REG             PIC  9(08)         VALUE ZEROS.
.      01  FILLER                      REDEFINES  WRK-DT-INCL-REG.
.          05  WRK-ANO-INCL-REG        PIC  9(04).
.          05  WRK-MES-INCL-REG        PIC  9(02).
DPS10      05  WRK-DIA-INCL-REG        PIC  9(02).

DPS10  01  WRK-DT-CORRENTE             PIC  9(08)         VALUE ZEROS.
.      01  FILLER                      REDEFINES  WRK-DT-CORRENTE.
.          05  WRK-ANO-CORR            PIC  9(04).
.          05  WRK-MES-CORR            PIC  9(02).
DPS10      05  WRK-DIA-CORR            PIC  9(02).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** TABELA AUXILIAR ***'.
      *----------------------------------------------------------------*

       01  TAB-LISTA-T957.
           07  TAB-LINHA-T957  OCCURS  30 TIMES.
               10  TAB-LBDATA          PIC  X(08)         VALUE SPACES.
               10  TAB-VALOR           PIC  X(20)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-7600-DATA-HORA.
           05  WRK-7600-DT-JULIANA    PIC  9(05) COMP-3   VALUE ZEROS.
           05  WRK-7600-DT-AAMMDD     PIC  9(07) COMP-3   VALUE ZEROS.
           05  WRK-7600-DT-AAAAMMDD   PIC  9(09) COMP-3   VALUE ZEROS.
           05  WRK-7600-TI-HHMMSS     PIC  9(07) COMP-3   VALUE ZEROS.
           05  WRK-7600-TI-HHMMSSMMMMMM
                                      PIC  9(13) COMP-3   VALUE ZEROS.
           05  WRK-7600-TIMESTAMP     PIC  X(20)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS DA BRAD1205 ***'.
      *----------------------------------------------------------------*

       01  WRK-1205-MENSAGEM           PIC  X(50)         VALUE SPACES.

       01  WRK-1205-LISTA-CAMPOS.
           05  WRK-1205-DT-ENVIADA     PIC  9(08)  COMP-3 VALUE ZEROS.
           05  WRK-1205-OPCAO          PIC  X(01)         VALUE SPACES.
           05  WRK-1205-DT-JULIANA     PIC  9(07)  COMP-3 VALUE ZEROS.
           05  WRK-1205-DT-EDITADA     PIC  X(10)         VALUE SPACES.
           05  WRK-1205-DT-GREGORI     PIC  9(08)         VALUE ZEROS.
           05  WRK-1205-DIA-SEMANA     PIC  X(13)         VALUE SPACES.
           05  WRK-1205-MES-EXTENSO    PIC  X(09)         VALUE SPACES.
           05  WRK-1205-DIA-UT-ANTER   PIC  9(08)         VALUE ZEROS.
           05  WRK-1205-DIA-UT-POSTE   PIC  9(08)         VALUE ZEROS.

PRI005*----------------------------------------------------------------*
PRI005 01  FILLER                      PIC  X(50)         VALUE
PRI005     '***  AREA PARA O PROGRAMA RUCA8516'.
PRI005*----------------------------------------------------------------*
PRI005
PRI005 COPY 'I#RUCAA3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS AUXILIARES PARA BRA7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS AUXILIARES PARA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           03  FILLER                  PIC  X(47)         VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660 *'.
           03  FILLER                  PIC  X(12)         VALUE
               '* RET.COD = '.
           03  WRK-RETURN-CODE         PIC  9(02)BB       VALUE ZEROS.
           03  FILLER                  PIC  X(08)         VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO          PIC  9(02)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS COMUNICACAO RUCA8808 ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-RUCA8808.
           05  WRK-8808-ENTRADA.
               07  WRK-8808-TERMINAL   PIC  X(08)         VALUE SPACES.
               07  WRK-8808-USUARIO    PIC  X(07)         VALUE SPACES.
               07  WRK-8808-CHAVE      PIC  X(09)         VALUE SPACES.
               07  WRK-8808-CTRLNAV    PIC  X(20)         VALUE SPACES.
           05  WRK-8808-SAIDA.
               07  WRK-8808-COD-RETORNO
                                       PIC  9(02)         VALUE ZEROS.
               07  WRK-8808-TEM-ALTERACAO
                                       PIC  X(01)         VALUE SPACES.
               07  WRK-8808-TEM-LIBERACAO
                                       PIC  X(01)         VALUE SPACES.

               07  WRK-8808-MENSAGEM   PIC  X(75)         VALUE SPACES.
               07  WRK-8808-ERRO-AREA  PIC  X(107)        VALUE SPACES.
               07  WRK-8808-SQLCA      PIC  X(136)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS COMUNICACAO RUCA8810 ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-RUCA8810.
           05  WRK-8810-ENTRADA.
               07  WRK-8810-USUARIO    PIC  X(07)         VALUE SPACES.
               07  WRK-8810-CHAVE      PIC  X(09)         VALUE SPACES.
               07  WRK-8810-CTRLNAV    PIC  X(20)         VALUE SPACES.
           05  WRK-8810-SAIDA.
               07  WRK-8810-COD-RETORNO
                                       PIC  9(02)         VALUE ZEROS.
               07  WRK-8810-MENSAGEM   PIC  X(75)         VALUE SPACES.
               07  WRK-8810-ERRO-AREA  PIC  X(107)        VALUE SPACES.
               07  WRK-8810-SQLCA      PIC  X(136)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS COMUNICACAO RUCA8814 ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-RUCA8814.
           05  WRK-8814-ENTRADA.
               07  WRK-8814-USUARIO    PIC  X(07)         VALUE SPACES.
               07  WRK-8814-CHAVE      PIC  X(09)         VALUE SPACES.
               07  WRK-8814-CTRLNAV    PIC  X(20)         VALUE SPACES.
STEFAB         07  WRK-8814-TERMINAL   PIC  X(08)         VALUE SPACES.
           05  WRK-8814-SAIDA.
               07  WRK-8814-COD-RETORNO
                                       PIC  9(02)         VALUE ZEROS.
               07  WRK-8814-MENSAGEM   PIC  X(75)         VALUE SPACES.
               07  WRK-8814-ERRO-AREA  PIC  X(107)        VALUE SPACES.
               07  WRK-8814-SQLCA      PIC  X(136)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREAS COMUNICACAO RUCA8812 ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-RUCA8812.
           05  WRK-8812-ENTRADA.
               07  WRK-8812-CHAVE      PIC  X(09)         VALUE SPACES.
               07  WRK-8812-CTRLNAV    PIC  X(20)         VALUE SPACES.
           05  WRK-8812-SAIDA.
               07  WRK-8812-COD-RETORNO
                                       PIC  9(02)         VALUE ZEROS.
               07  WRK-8812-MENSAGEM   PIC  X(75)         VALUE SPACES.
               07  WRK-8812-ERRO-AREA  PIC  X(107)        VALUE SPACES.
               07  WRK-8812-SQLCA      PIC  X(136)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA.
           05  WRK-COMU-LL             PIC  9(04) COMP    VALUE ZEROS.
           05  WRK-COMU-ZZ             PIC  9(04) COMP    VALUE ZEROS.
           05  WRK-COMU-TRANCODE       PIC  X(15)         VALUE SPACES.
           05  WRK-COMU-SENHAS         PIC  X(37)         VALUE SPACES.
           05  WRK-COMU-COMANDO        PIC  X(08)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** DADOS SALVOS DA TELA RUCAT951 ***'.
      *----------------------------------------------------------------*

       01  WRK-DADOS-NAV-T951.
           05  WRK-951-CTRLNAV         PIC  X(20)         VALUE SPACES.
           05  WRK-951-CAMPOS-ANT.
               07  WRK-951-CHAVE-ANT   PIC  9(09)         VALUE ZEROS.
               07  WRK-951-TPTITU-ANT  PIC  9(03)         VALUE ZEROS.
               07  WRK-951-TPPROD-ANT  PIC  9(02)         VALUE ZEROS.
STEFAB         07  WRK-951-CNPJ01-ANT  PIC  9(08)         VALUE ZEROS.
STEFAB         07  WRK-951-CNPJ02-ANT  PIC  9(04)         VALUE ZEROS.
STEFAB         07  WRK-951-CNPJ02-ANT  PIC  9(02)         VALUE ZEROS.
           05  WRK-951-CAMPOS-ATU.
               07  WRK-951-CHAVE-ATU   PIC  X(09)         VALUE SPACES.
               07  WRK-951-TPTITU-ATU  PIC  X(03)         VALUE SPACES.
               07  WRK-951-TPPROD-ATU  PIC  X(02)         VALUE SPACES.
STEFAB         07  WRK-951-CNPJ01-ATU  PIC  X(08)         VALUE ZEROS.
STEFAB         07  WRK-951-CNPJ02-ATU  PIC  X(04)         VALUE ZEROS.
STEFAB         07  WRK-951-CNPJ03-ATU  PIC  X(02)         VALUE ZEROS.
           05  WRK-951-CTRL-CPO-ALT.
               07  WRK-951-STA-TPTITU  PIC  X(01)         VALUE SPACES.
               07  WRK-951-STA-TPPROD  PIC  X(01)         VALUE SPACES.
STEFAB         07  WRK-951-STA-CNPJ    PIC  X(01)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** DADOS SALVOS DA TELA RUCAT955 ***'.
      *----------------------------------------------------------------*

       01  WRK-DADOS-NAV-T955.
           05  WRK-955-CTRLNAV         PIC  X(20)         VALUE SPACES.
           05  WRK-955-CHAVE           PIC  X(09)         VALUE SPACES.
           05  WRK-955-NUPARC          PIC  9(03)         VALUE ZEROS.
           05  WRK-955-DTVCTO          PIC  X(08)         VALUE SPACES.
           05  FILLER                  REDEFINES  WRK-955-DTVCTO.
               07  WRK-955-AAVCTO      PIC  9(04).
               07  WRK-955-MMVCTO      PIC  9(02).
               07  WRK-955-DDVCTO      PIC  9(02).
           05  WRK-955-SLPARC          PIC  9(13)V99      VALUE ZEROS.
           05  WRK-955-SALVO           PIC  X(01)         VALUE SPACES.
           05  WRK-955-DADOS-PARC-ANT  PIC  X(23)         VALUE SPACES.
           05  WRK-955-NOVA-PARC-ANT   PIC  X(22)         VALUE SPACES.
           05  WRK-955-DADOS-PARC-ATU.
               07  WRK-955-MOTIVO-ATU  PIC  X(05)         VALUE SPACES.
               07  WRK-955-NCONE-ATU.
                   10  WRK-955-NCONE1-ATU
                                       PIC  9(04)         VALUE ZEROS.
                   10  WRK-955-NCONE2-ATU
                                       PIC  9(05)         VALUE ZEROS.
                   10  WRK-955-NCONE3-ATU
                                       PIC  9(09)         VALUE ZEROS.
           05  WRK-955-NOVA-PARC-ATU.
               07  WRK-955-NVDATA-ATU
                                       PIC  X(08)         VALUE SPACES.
               07  WRK-955-VALOR-ATU   PIC  X(14)         VALUE SPACES.
               07  FILLER              REDEFINES  WRK-955-VALOR-ATU.
                   10  FILLER          PIC  X(03).
                   10  WRK-955-VALOR-ATU-N
                                       PIC  9(09)V99.
           05  WRK-955-STA-CPO-ALT.
               07  WRK-955-STA-NCONE   PIC  X(01)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** DADOS SALVOS DA TELA RUCAT957 ***'.
      *----------------------------------------------------------------*

       01  WRK-DADOS-NAV-T957.
           05  WRK-957-CTRLNAV         PIC  X(20)         VALUE SPACES.
           05  WRK-957-CHAVE           PIC  X(09)         VALUE SPACES.
           05  WRK-957-SALVO           PIC  X(01)         VALUE SPACES.
           05  WRK-957-NOVA-LIBE.
               07  WRK-957-LBDATA      PIC  X(08)         VALUE SPACES.
               07  WRK-957-VALOR       PIC  X(20)         VALUE SPACES.
               07  FILLER              REDEFINES  WRK-957-VALOR.
                   10  FILLER          PIC  X(05).
                   10  WRK-957-VALOR-N PIC  9(13)V99.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREA INPUT - TELA RUCAT957 ***'.
      *----------------------------------------------------------------*

       01  INPUT-T957.
           03  INP-LL-T957             PIC  9(04) COMP    VALUE ZEROS.
           03  INP-ZZ-T957             PIC  9(04) COMP    VALUE ZEROS.
           03  INP-TRANCODE-T957       PIC  X(08)         VALUE SPACES.
           03  FILLER                  PIC  X(06)         VALUE SPACES.
           03  INP-DADOS-T957.
               05  INP-PFK-T957        PIC  X(01)         VALUE SPACES.
               05  INP-SENHAS-T957     PIC  X(37)         VALUE SPACES.
               05  INP-COMANDO-T957    PIC  X(08)         VALUE SPACES.
               05  INP-TRANSA-T957     PIC  X(08)         VALUE SPACES.
               05  INP-CPONAV-T957.
                   07  INP-FASE-T957   PIC  9(01)         VALUE ZEROS.
                   07  INP-CHAVE-T957  PIC  X(09)         VALUE SPACES.
                   07  INP-CHAVE-T957-N     REDEFINES
                       INP-CHAVE-T957  PIC  9(09).
                   07  INP-CTRLNAV-T957.
                       10  INP-ANO-CTRLNAV-T957
                                       PIC  9(04)         VALUE ZEROS.
                       10  INP-MES-CTRLNAV-T957
                                       PIC  9(02)         VALUE ZEROS.
                       10  INP-DIA-CTRLNAV-T957
                                       PIC  9(02)         VALUE ZEROS.
                       10  INP-HOR-CTRLNAV-T957
                                       PIC  9(02)         VALUE ZEROS.
                       10  INP-MIN-CTRLNAV-T957
                                       PIC  9(02)         VALUE ZEROS.
                       10  INP-SEG-CTRLNAV-T957
                                       PIC  9(02)         VALUE ZEROS.
                       10  INP-MIL-CTRLNAV-T957
                                       PIC  9(06)         VALUE ZEROS.
               05  INP-ATRIB-T957.
                   07  INP-ATRIB1-T957 PIC  X(15)         VALUE ZEROS.
                   07  INP-ATRIB2-T957 PIC  X(18)         VALUE ZEROS.
                   07  INP-ATRIB3-T957 PIC  X(09)         VALUE ZEROS.
                   07  INP-ATRIB4-T957 PIC  X(10)         VALUE ZEROS.
                   07  INP-ATRIB5-T957 PIC  X(08)         VALUE ZEROS.
               05  FILLER              REDEFINES  INP-ATRIB-T957.
                   07  FILLER          OCCURS  30 TIMES.
                       10  INP-ATRIB-LBDATA
                                       PIC  9(01).
                       10  INP-ATRIB-VALOR
                                       PIC  9(01).
               05  INP-LISTA-T957.
                   07  INP-LINHA-T957  OCCURS  30 TIMES.
                       10  INP-LBDATA-T957.
                           15  INP-LBANO-T957
                                       PIC  X(04)         VALUE SPACES.
                           15  INP-LBMES-T957
                                       PIC  X(02)         VALUE SPACES.
                           15  INP-LBDIA-T957
                                       PIC  X(02)         VALUE SPACES.
                       10  INP-LBDATA-T957-N REDEFINES
                           INP-LBDATA-T957
                                       PIC  9(08).
                       10  INP-VALOR-T957
                                       PIC  X(20)         VALUE SPACES.
                       10  FILLER      REDEFINES  INP-VALOR-T957.
                           15  FILLER  PIC  X(05).
                           15  INP-VALOR-T957-N
                                       PIC  9(13)V99.
               05  INP-MENSAG-T957     PIC  X(79)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREA OUTPUT - TELA RUCAT957 ***'.
      *----------------------------------------------------------------*

       01  OUTPUT-T957.
           03  OUT-LL-T957             PIC S9(04) COMP    VALUE ZEROS.
           03  OUT-ZZ-T957             PIC  9(04) COMP    VALUE ZEROS.
           03  OUT-DADOS-T957.
               05  OUT-SENHAS-T957     PIC  X(37)         VALUE SPACES.
               05  OUT-COMANDO-T957    PIC  X(08)         VALUE SPACES.
               05  OUT-TRANSA-T957     PIC  X(08)         VALUE SPACES.
               05  OUT-CPONAV-T957     PIC  X(30)         VALUE SPACES.
               05  OUT-ATRIB-T957      PIC  X(60)         VALUE SPACES.
               05  OUT-LISTA-T957.
                   07  OUT-LINHA-T957  OCCURS  30 TIMES.
                       10  OUT-LBDATA-T957.
                           15  OUT-LBANO-ATTR-T957
                                       PIC S9(04) COMP    VALUE ZEROS.
                           15  OUT-LBANO-T957
                                       PIC  X(04)         VALUE SPACES.
                           15  OUT-LBMES-ATTR-T957
                                       PIC S9(04) COMP    VALUE ZEROS.
                           15  OUT-LBMES-T957
                                       PIC  X(02)         VALUE SPACES.
                           15  OUT-LBDIA-ATTR-T957
                                       PIC S9(04) COMP    VALUE ZEROS.
                           15  OUT-LBDIA-T957
                                       PIC  X(02)         VALUE SPACES.
                       10  OUT-VALOR-ATTR-T957
                                       PIC S9(04) COMP    VALUE ZEROS.
                       10  OUT-VALOR-T957
                                       PIC  X(20)         VALUE SPACES.
               05  OUT-MENSAG-T957     PIC  X(79)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '*** AREA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  660-RUCAT957.
           03  660-LL-AREA-T957        PIC  9(04) COMP    VALUE 0000.
           03  660-LL-MENSAGEM-T957    PIC  9(04) COMP    VALUE 0000.
           03  660-SENHAS-T957         PIC  9(04) COMP    VALUE 0037.
           03  660-COMANDO-T957        PIC  9(04) COMP    VALUE 0008.
           03  660-TRANSA-T957         PIC  9(04) COMP    VALUE 0008.
           03  660-CPONAV-T957         PIC  9(04) COMP    VALUE 0030.
           03  660-ATRIB1-T957         PIC  9(04) COMP    VALUE 0015.
           03  660-ATRIB2-T957         PIC  9(04) COMP    VALUE 0018.
           03  660-ATRIB3-T957         PIC  9(04) COMP    VALUE 0009.
           03  660-ATRIB4-T957         PIC  9(04) COMP    VALUE 0010.
           03  660-ATRIB5-T957         PIC  9(04) COMP    VALUE 0008.
           03  FILLER                  OCCURS 15 TIMES.
               05  660-LBANO1-T957     PIC  9(04) COMP    VALUE 2006.
               05  660-LBMES1-T957     PIC  9(04) COMP    VALUE 2004.
               05  660-LBDIA1-T957     PIC  9(04) COMP    VALUE 2004.
               05  660-VALOR1-T957     PIC  9(04) COMP    VALUE 2022.
               05  660-LBANO2-T957     PIC  9(04) COMP    VALUE 2006.
               05  660-LBMES2-T957     PIC  9(04) COMP    VALUE 2004.
               05  660-LBDIA2-T957     PIC  9(04) COMP    VALUE 2004.
               05  660-VALOR2-T957     PIC  9(04) COMP    VALUE 2022.
           03  660-MENSAG-T957         PIC  9(04) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS PARA TRATAR NULO ***'.
      *----------------------------------------------------------------*

       01  WRK-WRESUL-CONS-MDULO-NULL  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B004-DTEVENTO-NULL      PIC S9(04)  COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** TABELAS DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB004
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB014
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB015
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB048
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
               DECLARE  CSR01-RUECB048 CURSOR WITH HOLD FOR
                SELECT  CTERM             ,
                        CIDTFD_MDULO      ,
                        CSEQ_CTRL_MDULO   ,
                        WRESUL_CONS_MDULO
                  FROM  DB2PRD.TAPOIO_UTILZ_MDULO
                 WHERE  CTERM              = :RUECB048.CTERM
                   AND  CIDTFD_MDULO       = :RUECB048.CIDTFD-MDULO
                   AND  CSEQ_CTRL_MDULO   >= :RUECB048.CSEQ-CTRL-MDULO
           END-EXEC.

           EXEC SQL
               DECLARE  CSR02-RUCAB004 CURSOR WITH HOLD FOR
                SELECT  B004_CH_CONTR     ,
                        B004_CH_TIPO      ,
                        B004_CH_NUMERO    ,
                        B004_CONTROLE     ,
                        B004_DTEVENTO     ,
                        B004_CODLANC      ,
                        B004_DEBCRED      ,
                        B004_VLPRICOR
                  FROM  DB2PRD.RUCA_CAD_DEP
                 WHERE  B004_CH_CONTR      = :RUCAB004.B004-CH-CONTR
                   AND  B004_CH_TIPO      >= :RUCAB004.B004-CH-TIPO
                   AND  B004_CH_NUMERO    >= :RUCAB004.B004-CH-NUMERO
                 ORDER  BY  B004_CH_CONTR ,
                            B004_CH_TIPO  ,
                            B004_CH_NUMERO
           END-EXEC.

           EXEC SQL
               DECLARE  CSR03-RUCAB014 CURSOR WITH HOLD FOR
                SELECT  CCONTR            ,
                        HINCL_REG         ,
                        NMANUT_EFETU      ,
                        RCPO_MANUT_RECOR
                  FROM  DB2PRD.TLOG_MANUT_CONTR
                 WHERE  CCONTR            = :RUCAB014.CCONTR
                   AND  HINCL_REG         = :RUCAB014.HINCL-REG
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)         VALUE
           '***  FIM DA WORKING RUCA6957  ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'          USING  LNK-IO-PCB
                                           LNK-ALT-PCB.

           MOVE WRK-GU                 TO  WRK-FUNCAO.

      *----------------------------------------------------------------*
       0000-OBTER-MENSAGEM             SECTION.
      *----------------------------------------------------------------*

           CALL  'POOL5005'        USING   WRK-VERSAO
                                           WRK-FUNCAO
                                           LNK-IO-PCB
                                           LNK-ALT-PCB
                                           WRK-MENSAGEM
                                           WRK-TELA
                                           WRK-COD-USER
                                           WRK-COD-DEPTO.

           IF  RETURN-CODE          EQUAL  04 OR 12
               GOBACK
           END-IF.

           PERFORM  1000-PROCEDIMENTOS-INICIAIS
           PERFORM  2000-PROCESSAR-MENSAGEM

           GO                          TO  0000-OBTER-MENSAGEM.

      *----------------------------------------------------------------*
       0000-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCEDIMENTOS-INICIAIS     SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  INP-DADOS-T957
                                           OUT-DADOS-T957
                                           WRK-INCONSIS
                                           WRK-ERRO-DATA
                                           WRK-VLR-INCONSIS
                                           WRK-DEC-INCONSIS

           MOVE  'RUCAT957'            TO  WRK-TELA.

      *----------------------------------------------------------------*
       1000-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TRANSACAO    NOT EQUAL 'RUCA6957'
               PERFORM  3000-RECEBER-CONTROLE
           ELSE
               PERFORM  4000-PROCESSAR-RUCAT957
           END-IF.

           IF  WRK-TELA             EQUAL 'RUCAT957'
               PERFORM  5000-DEVOLVER-TELA
               PERFORM  6000-BRAD0660-RUCAT957
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-MENSAGEM(1:LENGTH OF  WRK-COMU-AREA)
                                       TO  WRK-COMU-AREA

           PERFORM  3005-SELECIONAR-DADOS-T951

           IF  WRK-951-CHAVE-ATU    EQUAL  SPACES
               PERFORM  4020-TRATAR-PF3
           ELSE
PRI005         MOVE WRK-951-CHAVE-ATU  TO  RUCAA3-E-CONTR-R
PRI005         PERFORM 3030-VERIF-LIQUID-CONTRATO
               IF  WRK-TRANSACAO    EQUAL 'RUCA6951'
                   PERFORM  3020-RECEBER-CONTROLE-RUCA6951
               ELSE
                   PERFORM  3010-MONTAR-TELA-INICIAL
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3005-SELECIONAR-DADOS-T951      SECTION.
      *----------------------------------------------------------------*

           MOVE  LNK-IO-LTERM          TO  CTERM           OF RUECB048
           MOVE  951                   TO  CIDTFD-MDULO    OF RUECB048
           MOVE  10000                 TO  CSEQ-CTRL-MDULO OF RUECB048

           PERFORM  4500-SELECT-RUECB048

           IF  SQLCODE              EQUAL  +100
               MOVE  SPACES            TO  WRK-DADOS-NAV-T951
           ELSE
               MOVE  WRESUL-CONS-MDULO-TEXT(1: WRESUL-CONS-MDULO-LEN)
                                       TO  WRK-DADOS-NAV-T951
           END-IF.

      *----------------------------------------------------------------*
       3005-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3010-MONTAR-TELA-INICIAL        SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  INP-DADOS-T957
           MOVE  WRK-COMU-SENHAS       TO  INP-SENHAS-T957
           MOVE  WRK-TRANSACAO         TO  INP-TRANSA-T957
           MOVE  WRK-951-CTRLNAV       TO  INP-CTRLNAV-T957
           MOVE  WRK-951-CHAVE-ATU     TO  INP-CHAVE-T957

           PERFORM  3015-MONTAR-LINHAS-TELA

           MOVE  1                     TO  INP-FASE-T957
PRI005     IF  RUCAA3-S-CONTR-LIQUIDADO  EQUAL  'SIM'
PRI005         MOVE  ALL '4'               TO  INP-ATRIB-T957
PRI005         MOVE  4                     TO  INP-ATRIB-LBDATA(1)
PRI005         MOVE  WRK-MSG28             TO  INP-MENSAG-T957
PRI005     ELSE
PRI005         MOVE  ALL '1'               TO  INP-ATRIB-T957
PRI005         MOVE  2                     TO  INP-ATRIB-LBDATA(1)
PRI005         MOVE  WRK-MSG01             TO  INP-MENSAG-T957
PRI005     END-IF.

      *----------------------------------------------------------------*
       3010-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3015-MONTAR-LINHAS-TELA         SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO  IND-LIN
           MOVE  957                   TO  CIDTFD-MDULO    OF RUECB048
           MOVE  10000                 TO  CSEQ-CTRL-MDULO OF RUECB048

           PERFORM  4510-OPEN-CSR01-RUECB048
           PERFORM  4515-FETCH-CSR01-RUECB048

           IF  WRK-957-CTRLNAV      EQUAL  WRK-951-CTRLNAV
               PERFORM
                 UNTIL  SQLCODE         EQUAL  +100

                   ADD  1              TO  IND-LIN

                   MOVE  WRK-957-LBDATA
                                       TO  INP-LBDATA-T957    (IND-LIN)
                   MOVE  WRK-957-VALOR TO  INP-VALOR-T957     (IND-LIN)

                   PERFORM  4515-FETCH-CSR01-RUECB048
               END-PERFORM

               PERFORM  4520-CLOSE-CSR01-RUECB048
           ELSE
               PERFORM  4520-CLOSE-CSR01-RUECB048
               MOVE  10000             TO  WRK-CSEQ-CTRL-MDULO
               PERFORM  4505-EXCLUIR-DADOS-NAV
           END-IF.

      *----------------------------------------------------------------*
       3015-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3020-RECEBER-CONTROLE-RUCA6951  SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  INP-DADOS-T957
           MOVE  WRK-TRANSACAO         TO  INP-TRANSA-T957
           MOVE  WRK-COMU-SENHAS       TO  INP-SENHAS-T957
           MOVE  WRK-951-CTRLNAV       TO  INP-CTRLNAV-T957
           MOVE  WRK-951-CHAVE-ATU     TO  INP-CHAVE-T957
PRI005     IF  RUCAA3-S-CONTR-LIQUIDADO  EQUAL  'SIM'
PRI005         MOVE  1                 TO  INP-FASE-T957
PRI005         MOVE  ALL '4'           TO  INP-ATRIB-T957
PRI005         MOVE  WRK-MSG28         TO  INP-MENSAG-T957
PRI005     ELSE
PRI005         MOVE  3                 TO  INP-FASE-T957
PRI005         MOVE  ALL '4'           TO  INP-ATRIB-T957
PRI005         MOVE  WRK-MSG12         TO  INP-MENSAG-T957
PRI005     END-IF.

      *----------------------------------------------------------------*
       3020-99-FIM.    EXIT.
      *----------------------------------------------------------------*

PRI005*----------------------------------------------------------------*
PRI005 3030-VERIF-LIQUID-CONTRATO      SECTION.
PRI005*----------------------------------------------------------------*
PRI005
PRI005* *** SE SALDO DEVEDOR MAIOR QUE ZEROS CONTRATO NAO LIQUIDADVS
PRI005      CALL  WRK-RUCA8516         USING  RUCAA3-REGISTRO
PRI005                                       ERRO-AREA
PRI005                                       SQLCA.
PRI005
PRI005     EVALUATE  RUCAA3-COD-RETORNO
PRI005
PRI005     WHEN  00
PRI005           CONTINUE
PRI005
PRI005     WHEN  99
PRI005           PERFORM 9999-ROTINA-ERRO
PRI005
PRI005     WHEN  OTHER
PRI005           MOVE 'APL'            TO  ERR-TIPO-ACESSO
PRI005           MOVE  RUCAA3-MSG-RETORNO
PRI005                                 TO  ERR-TEXTO
PRI005           PERFORM 9999-ROTINA-ERRO
PRI005
PRI005     END-EVALUATE.
PRI005
PRI005*----------------------------------------------------------------*
PRI005 3030-99-FIM.                    EXIT.
PRI005*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-PROCESSAR-RUCAT957         SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-MENSAGEM(1:LENGTH OF  INPUT-T957)
                                       TO  INPUT-T957

           IF  INP-PFK-T957         EQUAL  '2' OR 'B' OR 'D' OR
                                           'E' OR 'H'
               PERFORM  4010-TRATAR-PFK-COMUNS
           ELSE
PRI005         MOVE INP-CHAVE-T957-N   TO  RUCAA3-E-CONTR
PRI005         PERFORM 3030-VERIF-LIQUID-CONTRATO
               PERFORM  4035-TRATAR-FASES
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4010-TRATAR-PFK-COMUNS          SECTION.
      *----------------------------------------------------------------*

           EVALUATE  INP-PFK-T957

               WHEN  'H'
                     CONTINUE

               WHEN  '2'
                     MOVE  10000       TO  WRK-CSEQ-CTRL-MDULO
                     PERFORM  4505-EXCLUIR-DADOS-NAV
                     PERFORM  4015-TRATAR-PF2

               WHEN  'B'
                     PERFORM  4020-TRATAR-PF3

               WHEN  'D'
                     MOVE  10000       TO  WRK-CSEQ-CTRL-MDULO
                     PERFORM  4505-EXCLUIR-DADOS-NAV
                     PERFORM  4025-TRATAR-PF5

               WHEN  'E'
                     MOVE  10000       TO  WRK-CSEQ-CTRL-MDULO
                     PERFORM  4505-EXCLUIR-DADOS-NAV
                     PERFORM  4030-TRATAR-PF10
           END-EVALUATE.

      *----------------------------------------------------------------*
       4010-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4015-TRATAR-PF2                 SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  WRK-COMU-AREA

           MOVE  64                    TO  WRK-COMU-LL
           MOVE 'RUCA6957'             TO  WRK-COMU-TRANCODE
                                           WRK-TELA
           MOVE 'DISC'                 TO  WRK-COMU-COMANDO
           MOVE  WRK-CHNG              TO  WRK-FUNCAO
           MOVE  WRK-COMU-AREA         TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       4015-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4020-TRATAR-PF3                 SECTION.
      *----------------------------------------------------------------*

           IF  INP-FASE-T957        EQUAL  3
               MOVE  'RUCA6951'        TO  WRK-TELA
           ELSE
               MOVE  INP-TRANSA-T957   TO  WRK-TELA
           END-IF

           INITIALIZE                      WRK-COMU-AREA

           MOVE  64                    TO  WRK-COMU-LL
           MOVE  'RUCA6957'            TO  WRK-COMU-TRANCODE
           MOVE  WRK-CHNG              TO  WRK-FUNCAO
           MOVE  WRK-COMU-AREA         TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       4020-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4025-TRATAR-PF5                 SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-COMU-AREA

           MOVE  64                    TO  WRK-COMU-LL
           MOVE  'RUCA6957'            TO  WRK-COMU-TRANCODE
           MOVE  'SENH7010'            TO  WRK-TELA
           MOVE  'PFK05'               TO  WRK-COMU-COMANDO
           MOVE  WRK-CHNG              TO  WRK-FUNCAO
           MOVE  WRK-COMU-AREA         TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       4025-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4030-TRATAR-PF10                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-COMU-AREA

           MOVE  58                    TO  WRK-COMU-LL
           MOVE  'RUCA6957'            TO  WRK-COMU-TRANCODE
           MOVE  'RUCA6950'            TO  WRK-TELA
           MOVE  WRK-CHNG              TO  WRK-FUNCAO
           MOVE  WRK-COMU-AREA         TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       4030-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4035-TRATAR-FASES               SECTION.
      *----------------------------------------------------------------*

           EVALUATE  INP-FASE-T957

               WHEN  1
                     PERFORM  4100-TRATAR-FASE-1

               WHEN  2
                     PERFORM  4300-TRATAR-FASE-2

               WHEN  3
                     PERFORM  4400-TRATAR-FASE-3
           END-EVALUATE.

      *----------------------------------------------------------------*
       4035-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*

           EVALUATE  INP-PFK-T957

               WHEN  '.'
                     PERFORM  4105-TRATAR-ENTER-FASE-1

               WHEN  'F'
                     PERFORM   4190-TRATAR-PF11-FASE-1

               WHEN  OTHER
                     MOVE  WRK-MSG02   TO  INP-MENSAG-T957

           END-EVALUATE.

      *----------------------------------------------------------------*
       4100-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4105-TRATAR-ENTER-FASE-1        SECTION.
      *----------------------------------------------------------------*

PRI005* *** CONTRATO LIQUIDADO NAO PERMITE ALTERAR OS CAMPOS
PRI005* *** MANTEM SEMPRE NA FASE 1 E PERMITE USO DAS PF9 E PF11
PRI005     IF  RUCAA3-S-CONTR-LIQUIDADO  EQUAL  'SIM'
PRI005         MOVE  1                   TO  INP-FASE-T957
PRI005         MOVE  ALL '4'             TO  INP-ATRIB-T957
PRI005         MOVE WRK-MSG28            TO  INP-MENSAG-T957
PRI005     ELSE
               MOVE  1                     TO  INP-FASE-T957
               MOVE  ALL '1'               TO  INP-ATRIB-T957

               PERFORM  4110-CONSIST-FISICA-FASE-1
               VARYING  IND-LIN         FROM  1  BY 1
                 UNTIL  IND-LIN      GREATER  30

               IF  WRK-INCONSIS         EQUAL  SPACES
                   PERFORM  4125-CONSIST-LOGICA-FASE-1
               END-IF
PRI005     END-IF.

           IF  WRK-INCONSIS         EQUAL  SPACES
               PERFORM  4180-PREPARAR-FASE-2
           END-IF.

      *----------------------------------------------------------------*
       4105-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4110-CONSIST-FISICA-FASE-1      SECTION.
      *----------------------------------------------------------------*

           IF  INP-LBDATA-T957(IND-LIN) EQUAL  SPACES OR LOW-VALUES
               CONTINUE
           ELSE
               INSPECT  INP-LBANO-T957(IND-LIN)
                                       REPLACING  LEADING ' ' BY '0'
               INSPECT  INP-LBMES-T957(IND-LIN)
                                       REPLACING  LEADING ' ' BY '0'
               INSPECT  INP-LBDIA-T957(IND-LIN)
                                       REPLACING  LEADING ' ' BY '0'

               IF  INP-LBDATA-T957  (IND-LIN)     IS    NUMERIC  AND
                   INP-LBDATA-T957-N(IND-LIN)  GREATER  ZEROS
                   MOVE  INP-LBDATA-T957-N(IND-LIN)
                                       TO  WRK-1205-DT-ENVIADA
                   PERFORM  4115-VALIDAR-DATA

                   IF  WRK-ERRO-DATA   EQUAL  'S'
                       MOVE  'S'       TO  WRK-INCONSIS
                       MOVE  3         TO  INP-ATRIB-LBDATA(IND-LIN)
                       MOVE  WRK-MSG03 TO  INP-MENSAG-T957
                   END-IF
               ELSE
                   MOVE  'S'           TO  WRK-INCONSIS
                   MOVE  3             TO  INP-ATRIB-LBDATA(IND-LIN)
                   MOVE  WRK-MSG03     TO  INP-MENSAG-T957
               END-IF
           END-IF.

      *----------------------------------------------------------------*

           IF  INP-VALOR-T957(IND-LIN) EQUAL  SPACES OR LOW-VALUES
               CONTINUE
           ELSE
               INSPECT  INP-VALOR-T957(IND-LIN)
                                       REPLACING  LEADING ' ' BY '0'

               MOVE  INP-VALOR-T957(IND-LIN)
                                       TO  WRK-VALOR-ENT
               MOVE  20                TO  IND-TAM

               PERFORM  4120-TRATAR-VLR-ED

               EVALUATE  TRUE
               WHEN  WRK-VLR-INCONSIS   EQUAL  'S'
                     MOVE  'S'         TO  WRK-INCONSIS
                     MOVE  3           TO  INP-ATRIB-VALOR(IND-LIN)
                     MOVE  WRK-MSG03   TO  INP-MENSAG-T957

               WHEN  WRK-DEC-INCONSIS  EQUAL  'S'     AND
                     WRK-INCONSIS      EQUAL  SPACES
                     MOVE  'S'         TO  WRK-INCONSIS
                     MOVE  9           TO  INP-ATRIB-VALOR(IND-LIN)
                     MOVE  WRK-MSG04   TO  INP-MENSAG-T957

               WHEN  WRK-VALOR-SAI     IS  NUMERIC  AND
                     WRK-DEC-INCONSIS  EQUAL  SPACES
                     MOVE  WRK-VALOR-SAI
                                       TO  INP-VALOR-T957(IND-LIN)
               END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
       4110-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4115-VALIDAR-DATA               SECTION.
      *----------------------------------------------------------------*

           MOVE  'N'                   TO  WRK-ERRO-DATA

           CALL  'BRAD1205'         USING  WRK-1205-LISTA-CAMPOS
                                           WRK-1205-MENSAGEM

           IF  RETURN-CODE      NOT EQUAL  ZEROS
               MOVE  'S'               TO  WRK-ERRO-DATA
           END-IF.

      *----------------------------------------------------------------*
       4115-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4120-TRATAR-VLR-ED              SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  WRK-VLR-INCONSIS
                                           WRK-DEC-INCONSIS
           MOVE  ZEROS                 TO  WRK-VALOR-SAI
           MOVE  20                    TO  IND-POSI

           COMPUTE  IND-MASC           =   (20 - IND-TAM) + 1

           PERFORM
           VARYING  IND-CHAR         FROM  1 BY 1
             UNTIL  IND-CHAR      GREATER  IND-TAM

             EVALUATE  TRUE

             WHEN  WRK-MASCARA-ED(IND-MASC: 1)  EQUAL  'Z'

                   IF  WRK-VALOR-ENT(IND-CHAR: 1)  IS  NUMERIC
                       CONTINUE
                   ELSE
                       MOVE  'S'       TO  WRK-VLR-INCONSIS
                   END-IF

             WHEN  WRK-MASCARA-ED(IND-MASC: 1)  EQUAL  '.'

                   IF  WRK-VALOR-ENT(IND-CHAR: 1)  IS    NUMERIC  OR
                       WRK-VALOR-ENT(IND-CHAR: 1)  EQUAL '.'
                       CONTINUE
                   ELSE
                       MOVE  'S'       TO  WRK-VLR-INCONSIS
                   END-IF

             WHEN  WRK-MASCARA-ED(IND-MASC: 1)  EQUAL  ','

                   IF  WRK-VALOR-ENT(IND-CHAR: 1)  EQUAL  ','
                       CONTINUE
                   ELSE
                       MOVE  'S'       TO  WRK-DEC-INCONSIS
                   END-IF

             END-EVALUATE

             ADD  1                    TO  IND-MASC
           END-PERFORM.

           IF  WRK-VLR-INCONSIS     EQUAL  SPACES  AND
               WRK-DEC-INCONSIS     EQUAL  SPACES
               PERFORM
               VARYING  IND-CHAR     FROM  IND-TAM BY -1
                 UNTIL  IND-CHAR     LESS  1

                 IF  WRK-VALOR-ENT (IND-CHAR: 1) IS    NUMERIC
                     MOVE  WRK-VALOR-ENT  (IND-CHAR: 1)  TO
                           WRK-VALOR-SAI  (IND-POSI: 1)
                     SUBTRACT  1     FROM  IND-POSI
                 END-IF
               END-PERFORM
           END-IF.

      *----------------------------------------------------------------*
       4120-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4125-CONSIST-LOGICA-FASE-1      SECTION.
      *----------------------------------------------------------------*

           PERFORM  4130-VALIDAR-DATA-VALOR

           IF  WRK-INCONSIS         EQUAL  SPACES
               PERFORM  4135-OBTER-DATA-HORA

               PERFORM
               VARYING  IND-LIN          FROM  1  BY 1
                 UNTIL  IND-LIN       GREATER  30

                    IF  INP-LBDATA-T957  (IND-LIN)      IS  NUMERIC  AND
                        INP-LBDATA-T957-N(IND-LIN) GREATER  ZEROS
                        PERFORM  4140-VALIDAR-DATA-UTIL
                    END-IF
               END-PERFORM
           END-IF.

      *----------------------------------------------------------------*

           IF  WRK-INCONSIS         EQUAL  SPACES
               PERFORM  4145-OBTER-PARC-PGTO-ORIGINAL
               PERFORM  4165-OBTER-PARC-PGTO-ALTERADA

               PERFORM
               VARYING  IND-LIN          FROM  1  BY 1
                 UNTIL  IND-LIN       GREATER  30

                    IF  INP-LBDATA-T957  (IND-LIN)      IS  NUMERIC  AND
                        INP-LBDATA-T957-N(IND-LIN) GREATER  ZEROS
                        PERFORM  4170-VALIDAR-COM-DATA-PGTO
                    END-IF
               END-PERFORM
           END-IF.

      *----------------------------------------------------------------*

           IF  WRK-INCONSIS         EQUAL  SPACES
               PERFORM  4175-SELECT-RUFIB002

               IF  VFINAN OF RUFIB002 LESS  ACU-VALOR-TOTAL
                   MOVE  'S'           TO  WRK-INCONSIS
                   MOVE  3             TO  INP-ATRIB-LBDATA(1)
                   MOVE  WRK-MSG26     TO  INP-MENSAG-T957
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       4125-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4130-VALIDAR-DATA-VALOR         SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO  ACU-VALOR-TOTAL

           PERFORM
           VARYING  IND-LIN          FROM  1 BY 1
             UNTIL  IND-LIN       GREATER  30

             IF  INP-LBDATA-T957  (IND-LIN)     IS    NUMERIC  AND
                 INP-LBDATA-T957-N(IND-LIN)  GREATER  ZEROS

                 IF  INP-VALOR-T957  (IND-LIN)     IS    NUMERIC  AND
                     INP-VALOR-T957-N(IND-LIN)  GREATER  ZEROS
                     MOVE  INP-VALOR-T957-N(IND-LIN)
                                       TO  ACU-VALOR-AUX

                     COMPUTE  ACU-VALOR-TOTAL = ACU-VALOR-TOTAL
                                              + ACU-VALOR-AUX
                 ELSE
                     MOVE  'S'         TO  WRK-INCONSIS
                     MOVE  3           TO  INP-ATRIB-VALOR(IND-LIN)
                     MOVE  WRK-MSG06   TO  INP-MENSAG-T957
                 END-IF
             ELSE
                 IF  INP-VALOR-T957  (IND-LIN)     IS    NUMERIC  AND
                     INP-VALOR-T957-N(IND-LIN)  GREATER  ZEROS
                     MOVE  'S'         TO  WRK-INCONSIS
                     MOVE  3           TO  INP-ATRIB-LBDATA(IND-LIN)
                     MOVE  WRK-MSG06   TO  INP-MENSAG-T957
                 END-IF
             END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       4130-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4135-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-7600-DATA-HORA

           CALL 'BRAD7600'          USING  WRK-7600-DATA-HORA.

           MOVE  WRK-7600-DT-AAAAMMDD  TO  WRK-S09-DRESS
DPS10                                      WRK-DT-CORRENTE
           MOVE  WRK-X08-DRESS         TO  WRK-DATA-ATUAL.


      *----------------------------------------------------------------*
       4135-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4140-VALIDAR-DATA-UTIL           SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-LBDATA-T957-N(IND-LIN)
                                       TO  WRK-1205-DT-ENVIADA
           PERFORM  4115-VALIDAR-DATA

           IF  WRK-1205-OPCAO  EQUAL  'U'
               CONTINUE
           ELSE
              MOVE  'S'           TO  WRK-INCONSIS
              MOVE  3             TO  INP-ATRIB-LBDATA(IND-LIN)
              MOVE  WRK-MSG07     TO  INP-MENSAG-T957
           END-IF.

      *----------------------------------------------------------------*
       4140-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4145-OBTER-PARC-PGTO-ORIGINAL   SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO  WRK-PRIMEIRA-PARC

           PERFORM  4150-OPEN-CSR02-RUCAB004
           PERFORM  4155-FETCH-CSR02-RUCAB004

           PERFORM
             UNTIL  SQLCODE              EQUAL  +100
                OR  WRK-PRIMEIRA-PARC  GREATER  ZEROS

                IF  B004-CODLANC  OF RUCAB004      LESS   23         AND
                    B004-DEBCRED  OF RUCAB004      EQUAL '6'
                    MOVE  B004-CH-NUMERO   OF  RUCAB004
                                       TO  WRK-S09-DRESS
                    MOVE  WRK-N03-DRESS
                                       TO  WRK-PARC-PRI

                    MOVE  B004-DTEVENTO    OF  RUCAB004
                                       TO  WRK-DATA-DB2
                    MOVE  WRK-DIA-DB2  TO  WRK-DIA-DTEVENTO
                    MOVE  WRK-MES-DB2  TO  WRK-MES-DTEVENTO
                    MOVE  WRK-ANO-DB2  TO  WRK-ANO-DTEVENTO
                    MOVE  WRK-DATA-DTEVENTO
                                       TO  WRK-DTVCTO-PRI
               END-IF

               PERFORM  4155-FETCH-CSR02-RUCAB004
           END-PERFORM.

           PERFORM  4160-CLOSE-CSR02-RUCAB004.

      *----------------------------------------------------------------*
       4145-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4150-OPEN-CSR02-RUCAB004        SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-CHAVE-T957-N      TO  B004-CH-CONTR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-CH-TIPO     OF RUCAB004
           MOVE  1                     TO  B004-CH-NUMERO   OF RUCAB004

           EXEC SQL
                OPEN CSR02-RUCAB004
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0001'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4150-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4155-FETCH-CSR02-RUCAB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR02-RUCAB004
                INTO   :RUCAB004.B004-CH-CONTR     ,
                       :RUCAB004.B004-CH-TIPO      ,
                       :RUCAB004.B004-CH-NUMERO    ,
                       :RUCAB004.B004-CONTROLE     ,
                       :RUCAB004.B004-DTEVENTO
                          :WRK-B004-DTEVENTO-NULL  ,
                       :RUCAB004.B004-CODLANC      ,
                       :RUCAB004.B004-DEBCRED      ,
                       :RUCAB004.B004-VLPRICOR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0002'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               IF  WRK-B004-DTEVENTO-NULL  EQUAL  -1
                   MOVE  ZEROS         TO  B004-DTEVENTO    OF RUCAB004
               END-IF
           ELSE
               MOVE  ZEROS             TO  B004-CH-NUMERO   OF RUCAB004
                                           B004-DTEVENTO    OF RUCAB004
                                           B004-VLPRICOR    OF RUCAB004
           END-IF.

      *----------------------------------------------------------------*
       4155-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4160-CLOSE-CSR02-RUCAB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR02-RUCAB004
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0003'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4160-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4165-OBTER-PARC-PGTO-ALTERADA   SECTION.
      *----------------------------------------------------------------*

           MOVE  '29991231'            TO  WRK-DTVCTO-AUX
           MOVE  955                   TO  CIDTFD-MDULO    OF RUECB048
           MOVE  20000                 TO  CSEQ-CTRL-MDULO OF RUECB048

           PERFORM  4510-OPEN-CSR01-RUECB048
           PERFORM  4515-FETCH-CSR01-RUECB048

           PERFORM
             UNTIL  SQLCODE              EQUAL  +100

                MOVE  WRESUL-CONS-MDULO-TEXT(1: WRESUL-CONS-MDULO-LEN)
                                       TO  WRK-DADOS-NAV-T955

                IF  WRK-955-SALVO   EQUAL  'S'          AND
                    WRK-955-NUPARC  EQUAL  WRK-PARC-PRI

                    IF  WRK-955-NVDATA-ATU GREATER  ZEROS          AND
                        WRK-955-NVDATA-ATU    LESS  WRK-DTVCTO-AUX
                        MOVE  WRK-955-NVDATA-ATU
                                       TO  WRK-DTVCTO-AUX
                    END-IF
               END-IF

               PERFORM  4515-FETCH-CSR01-RUECB048
           END-PERFORM.

           PERFORM  4520-CLOSE-CSR01-RUECB048.

      *----------------------------------------------------------------*
       4165-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4170-VALIDAR-COM-DATA-PGTO     SECTION.
      *----------------------------------------------------------------*

           IF  WRK-DTVCTO-AUX                 LESS  '29991231'
               IF  INP-LBDATA-T957(IND-LIN) GREATER  WRK-DTVCTO-AUX
                   MOVE  'S'           TO  WRK-INCONSIS
                   MOVE  3             TO  INP-ATRIB-LBDATA(IND-LIN)
                   MOVE  WRK-MSG08     TO  INP-MENSAG-T957
               END-IF
           ELSE
               IF  INP-LBDATA-T957(IND-LIN) GREATER WRK-DTVCTO-PRI
                   MOVE  'S'           TO  WRK-INCONSIS
                   MOVE  3             TO  INP-ATRIB-LBDATA(IND-LIN)
                   MOVE  WRK-MSG08     TO  INP-MENSAG-T957
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       4170-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4175-SELECT-RUFIB002            SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-CHAVE-T957-N      TO  CCONTR OF RUFIB002

           EXEC SQL
                SELECT
                        VFINAN
                  INTO
                        :RUFIB002.VFINAN
                  FROM  DB2PRD.CONTRATO_RURAL
                 WHERE  CCONTR  = :RUFIB002.CCONTR

           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS   )  OR
              ( SQLCODE             EQUAL  +100    )  OR
              ( SQLWARN0            EQUAL  'W'     )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'   TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0004'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4175-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4180-PREPARAR-FASE-2            SECTION.
      *----------------------------------------------------------------*

           PERFORM  4185-CARREGAR-TAB-AUX

           PERFORM
           VARYING  IND-LIN          FROM  1 BY 1
             UNTIL  IND-LIN       GREATER  30
                OR  INP-FASE-T957   EQUAL  2

             EVALUATE TRUE

             WHEN ( INP-LBDATA-T957(IND-LIN)
                                    EQUAL SPACES OR LOW-VALUES )  AND
                  ( TAB-LBDATA     (IND-LIN)
                                    EQUAL SPACES OR LOW-VALUES )  AND
                  ( INP-VALOR-T957 (IND-LIN)
                                    EQUAL SPACES OR LOW-VALUES )  AND
                  ( TAB-VALOR      (IND-LIN)
                                    EQUAL SPACES OR LOW-VALUES )
                   CONTINUE

             WHEN  INP-LBDATA-T957(IND-LIN)
                                    EQUAL  TAB-LBDATA(IND-LIN)    AND
                   INP-VALOR-T957 (IND-LIN)
                                    EQUAL  TAB-VALOR (IND-LIN)
                   CONTINUE

             WHEN  OTHER
                   MOVE  2             TO   INP-FASE-T957

             END-EVALUATE
           END-PERFORM.

           IF  INP-FASE-T957        EQUAL  2
               MOVE  ALL '4'           TO  INP-ATRIB-T957
               MOVE  WRK-MSG10         TO  INP-MENSAG-T957
           ELSE
               MOVE  WRK-MSG01         TO  INP-MENSAG-T957
           END-IF.

      *----------------------------------------------------------------*
       4180-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4185-CARREGAR-TAB-AUX           SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  TAB-LISTA-T957
           MOVE  ZEROS                 TO  IND-LIN
           MOVE  957                   TO  CIDTFD-MDULO    OF RUECB048
           MOVE  10000                 TO  CSEQ-CTRL-MDULO OF RUECB048

           PERFORM  4510-OPEN-CSR01-RUECB048
           PERFORM  4515-FETCH-CSR01-RUECB048

           PERFORM
             UNTIL  SQLCODE         EQUAL  +100

              ADD   1                  TO  IND-LIN

              MOVE  WRK-957-NOVA-LIBE
                                       TO  TAB-LINHA-T957(IND-LIN)
              PERFORM  4515-FETCH-CSR01-RUECB048
           END-PERFORM.

           PERFORM  4520-CLOSE-CSR01-RUECB048.

      *----------------------------------------------------------------*
       4185-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4190-TRATAR-PF11-FASE-1         SECTION.
      *----------------------------------------------------------------*

           PERFORM  4105-TRATAR-ENTER-FASE-1

           IF  INP-FASE-T957        EQUAL  2
               MOVE  1                 TO  INP-FASE-T957
               MOVE  ALL '1'           TO  INP-ATRIB-T957
               MOVE  2                 TO  INP-ATRIB-LBDATA(1)
               MOVE  WRK-MSG09         TO  INP-MENSAG-T957
           ELSE
               IF  WRK-INCONSIS     EQUAL  SPACES
                   PERFORM  4195-ATUALIZAR-BASES-MANUT

                   IF  WRK-8808-TEM-ALTERACAO  EQUAL  'S'  OR
                       WRK-8808-TEM-LIBERACAO  EQUAL  'S'
                       PERFORM  4200-ENVIAR-RCOR
                   ELSE
                       MOVE  1         TO  INP-FASE-T957
                       MOVE  ALL '1'   TO  INP-ATRIB-T957
                       MOVE  2         TO  INP-ATRIB-LBDATA(1)
                       MOVE  WRK-MSG27 TO  INP-MENSAG-T957
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       4190-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4195-ATUALIZAR-BASES-MANUT      SECTION.
      *----------------------------------------------------------------*

      *--> SALVA AS INFORMACOES DA BASE TEMPORARIA NAS BASES DE
      *    MANUTENCAO DO CONTRATO RECOR.

           INITIALIZE                      WRK-AREA-RUCA8808

           MOVE  LNK-IO-LTERM          TO  WRK-8808-TERMINAL
           MOVE  WRK-COD-USER          TO  WRK-8808-USUARIO
           MOVE  INP-CHAVE-T957        TO  WRK-8808-CHAVE
           MOVE  INP-CTRLNAV-T957      TO  WRK-8808-CTRLNAV
           MOVE  'RUCA8808'            TO  WRK-MODULO

           CALL  WRK-MODULO         USING  WRK-AREA-RUCA8808

           EVALUATE  WRK-8808-COD-RETORNO

           WHEN  00
                 CONTINUE

           WHEN  99
                 MOVE  WRK-8808-ERRO-AREA
                                       TO  ERRO-AREA
                 MOVE  WRK-8808-SQLCA  TO  SQLCA
                 PERFORM 9999-ROTINA-ERRO

           WHEN  OTHER
                 MOVE 'APL'            TO  ERR-TIPO-ACESSO
                 MOVE  WRK-8808-MENSAGEM
                                       TO ERR-TEXTO
                 PERFORM 9999-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       4195-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4200-ENVIAR-RCOR                SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO  WRK-8810-COD-RETORNO
                                           WRK-8814-COD-RETORNO
                                           WRK-8812-COD-RETORNO

           IF  WRK-8808-TEM-ALTERACAO  EQUAL  'S'
      *-->     FORMATA MENSAGEM COR0001 PARA ENVIO
               PERFORM  4205-CHAMAR-RUCA8810
           END-IF.

           IF  WRK-8808-TEM-LIBERACAO  EQUAL  'S'  AND
               WRK-8810-COD-RETORNO    EQUAL  ZEROS
      *-->     FORMATA MENSAGEM COR0005 PARA ENVIO
               PERFORM  4210-CHAMAR-RUCA8814
           END-IF.

           IF  WRK-8810-COD-RETORNO EQUAL  ZEROS  AND
               WRK-8814-COD-RETORNO EQUAL  ZEROS
      *-->     ENVIA PARA O RCOR MENSAGEM COR0001 OU COR0005
               PERFORM  4215-CHAMAR-RUCA8812
           END-IF.

           IF  WRK-8810-COD-RETORNO EQUAL  ZEROS  AND
               WRK-8814-COD-RETORNO EQUAL  ZEROS  AND
               WRK-8812-COD-RETORNO EQUAL  ZEROS
               MOVE  3                 TO  INP-FASE-T957
               MOVE  ALL '4'           TO  INP-ATRIB-T957
               MOVE  WRK-MSG25         TO  INP-MENSAG-T957
           END-IF.

      *----------------------------------------------------------------*
       4200-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4205-CHAMAR-RUCA8810            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-AREA-RUCA8810

           MOVE  WRK-COD-USER          TO  WRK-8810-USUARIO
           MOVE  INP-CHAVE-T957        TO  WRK-8810-CHAVE
           MOVE  INP-CTRLNAV-T957      TO  WRK-8810-CTRLNAV
######     MOVE  'RUCA8810'            TO  WRK-MODULO
######*   XMOVE  'RUCAX810'            TO  WRK-MODULO

           CALL  WRK-MODULO         USING  WRK-AREA-RUCA8810

           EVALUATE  WRK-8810-COD-RETORNO

           WHEN  00
                 CONTINUE

           WHEN  99
                 MOVE  WRK-8810-ERRO-AREA
                                       TO  ERRO-AREA
                 MOVE  WRK-8810-SQLCA  TO  SQLCA
                 PERFORM 9999-ROTINA-ERRO

           WHEN  OTHER
                 MOVE  3               TO  INP-FASE-T957
                 MOVE  ALL '4'         TO  INP-ATRIB-T957
                 MOVE  WRK-8810-MENSAGEM
                                       TO  INP-MENSAG-T957
                 MOVE  INP-CHAVE-T957-N
                                       TO  CCONTR           OF RUCAB015
                 MOVE  INP-ANO-CTRLNAV-T957
                                       TO  WRK-ANO-TIMES
                 MOVE  INP-MES-CTRLNAV-T957
                                       TO  WRK-MES-TIMES
                 MOVE  INP-DIA-CTRLNAV-T957
                                       TO  WRK-DIA-TIMES
                 MOVE  INP-HOR-CTRLNAV-T957
                                       TO  WRK-HOR-TIMES
                 MOVE  INP-MIN-CTRLNAV-T957
                                       TO  WRK-MIN-TIMES
                 MOVE  INP-SEG-CTRLNAV-T957
                                       TO  WRK-SEG-TIMES
                 MOVE  INP-MIL-CTRLNAV-T957
                                       TO  WRK-MIL-TIMES
                 MOVE  WRK-TIMESTAMP
                                       TO  HINCL-REG        OF RUCAB015
                 PERFORM  4420-EFETUAR-CANCELAMENTO
           END-EVALUATE.

      *----------------------------------------------------------------*
       4205-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4210-CHAMAR-RUCA8814            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-AREA-RUCA8814

STEFAB     MOVE  LNK-IO-LTERM          TO  WRK-8814-TERMINAL
           MOVE  WRK-COD-USER          TO  WRK-8814-USUARIO
           MOVE  INP-CHAVE-T957        TO  WRK-8814-CHAVE
           MOVE  INP-CTRLNAV-T957      TO  WRK-8814-CTRLNAV
           MOVE  'RUCA8814'            TO  WRK-MODULO

           CALL  WRK-MODULO         USING  WRK-AREA-RUCA8814

           EVALUATE  WRK-8814-COD-RETORNO

           WHEN  00
                 CONTINUE

           WHEN  99
                 MOVE  WRK-8814-ERRO-AREA
                                       TO  ERRO-AREA
                 MOVE  WRK-8814-SQLCA  TO  SQLCA
                 PERFORM 9999-ROTINA-ERRO

           WHEN  OTHER
                 MOVE  3               TO  INP-FASE-T957
                 MOVE  ALL '4'         TO  INP-ATRIB-T957
                 MOVE  WRK-8814-MENSAGEM
                                       TO  INP-MENSAG-T957
                 MOVE  INP-CHAVE-T957-N
                                       TO  CCONTR           OF RUCAB015
                 MOVE  INP-ANO-CTRLNAV-T957
                                       TO  WRK-ANO-TIMES
                 MOVE  INP-MES-CTRLNAV-T957
                                       TO  WRK-MES-TIMES
                 MOVE  INP-DIA-CTRLNAV-T957
                                       TO  WRK-DIA-TIMES
                 MOVE  INP-HOR-CTRLNAV-T957
                                       TO  WRK-HOR-TIMES
                 MOVE  INP-MIN-CTRLNAV-T957
                                       TO  WRK-MIN-TIMES
                 MOVE  INP-SEG-CTRLNAV-T957
                                       TO  WRK-SEG-TIMES
                 MOVE  INP-MIL-CTRLNAV-T957
                                       TO  WRK-MIL-TIMES
                 MOVE  WRK-TIMESTAMP
                                       TO  HINCL-REG        OF RUCAB015
                 PERFORM  4420-EFETUAR-CANCELAMENTO
           END-EVALUATE.

      *----------------------------------------------------------------*
       4210-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4215-CHAMAR-RUCA8812            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-AREA-RUCA8812

           MOVE  INP-CHAVE-T957        TO  WRK-8812-CHAVE
           MOVE  INP-CTRLNAV-T957      TO  WRK-8812-CTRLNAV
           MOVE  'RUCA8812'            TO  WRK-MODULO

           CALL  WRK-MODULO         USING  WRK-AREA-RUCA8812

           EVALUATE  WRK-8812-COD-RETORNO

           WHEN  00
                 CONTINUE

           WHEN  99
                 MOVE  WRK-8812-ERRO-AREA
                                       TO  ERRO-AREA
                 MOVE  WRK-8812-SQLCA  TO  SQLCA
                 PERFORM 9999-ROTINA-ERRO

           WHEN  OTHER
                 MOVE  3               TO  INP-FASE-T957
                 MOVE  ALL '4'         TO  INP-ATRIB-T957
                 MOVE  WRK-8812-MENSAGEM
                                       TO  INP-MENSAG-T957
           END-EVALUATE.

      *----------------------------------------------------------------*
       4215-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4300-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           EVALUATE  INP-PFK-T957

               WHEN  '.'
                     PERFORM  4305-TRATAR-ENTER-FASE-2

               WHEN  '6'
                     PERFORM  4310-TRATAR-PF6-FASE-2

               WHEN  OTHER
                     MOVE  WRK-MSG02   TO  INP-MENSAG-T957

           END-EVALUATE.

      *----------------------------------------------------------------*
       4300-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4305-TRATAR-ENTER-FASE-2        SECTION.
      *----------------------------------------------------------------*

           MOVE  1                     TO  INP-FASE-T957
           MOVE  ALL '1'               TO  INP-ATRIB-T957
           MOVE  2                     TO  INP-ATRIB-LBDATA(1)
           MOVE  WRK-MSG01             TO  INP-MENSAG-T957.

      *----------------------------------------------------------------*
       4305-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4310-TRATAR-PF6-FASE-2          SECTION.
      *----------------------------------------------------------------*

           MOVE  10000                 TO  WRK-CSEQ-CTRL-MDULO

           PERFORM  4505-EXCLUIR-DADOS-NAV
           PERFORM  4315-TRATAR-ALTERACAO

           MOVE  SPACES                TO  INP-LISTA-T957
           MOVE  INP-CTRLNAV-T957      TO  WRK-951-CTRLNAV

           PERFORM  3015-MONTAR-LINHAS-TELA

           MOVE  1                     TO  INP-FASE-T957
           MOVE  ALL '1'               TO  INP-ATRIB-T957
           MOVE  2                     TO  INP-ATRIB-LBDATA(1)
           MOVE  WRK-MSG11             TO  INP-MENSAG-T957.

      *----------------------------------------------------------------*
       4310-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4315-TRATAR-ALTERACAO          SECTION.
      *----------------------------------------------------------------*

           MOVE  10000                 TO  ACU-REGISTRO

           PERFORM
           VARYING  IND-LIN        FROM  1 BY 1
             UNTIL  IND-LIN     GREATER  30

             IF  INP-LBDATA-T957  (IND-LIN)     IS    NUMERIC  AND
                 INP-LBDATA-T957-N(IND-LIN)  GREATER  ZEROS

                 INSPECT  INP-VALOR-T957(IND-LIN)
                                       REPLACING  LEADING ' ' BY '0'

                 MOVE  INP-VALOR-T957 (IND-LIN)
                                       TO  WRK-VALOR-ENT
                 MOVE  20              TO  IND-TAM

                 PERFORM  4120-TRATAR-VLR-ED

                 MOVE  WRK-VALOR-SAI   TO  INP-VALOR-T957 (IND-LIN)

                 PERFORM  4320-SALVAR-ALTERACAO
             END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       4315-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4320-SALVAR-ALTERACAO           SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-CTRLNAV-T957      TO  WRK-957-CTRLNAV
           MOVE  INP-CHAVE-T957        TO  WRK-957-CHAVE
           MOVE  'S'                   TO  WRK-957-SALVO
           MOVE  INP-LBDATA-T957(IND-LIN)
                                       TO  WRK-957-LBDATA
           MOVE  INP-VALOR-T957 (IND-LIN)
                                       TO  WRK-957-VALOR

           ADD   1                     TO  ACU-REGISTRO

           MOVE  ACU-REGISTRO          TO  WRK-CSEQ-CTRL-MDULO
           MOVE  LNK-IO-LTERM          TO  CTERM           OF RUECB048
           MOVE  957                   TO  CIDTFD-MDULO    OF RUECB048
           MOVE  WRK-CSEQ-CTRL-MDULO   TO  CSEQ-CTRL-MDULO OF RUECB048

           PERFORM  4530-INSERT-RUECB048.

      *----------------------------------------------------------------*
       4320-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4400-TRATAR-FASE-3              SECTION.
      *----------------------------------------------------------------*

           EVALUATE  INP-PFK-T957

               WHEN  '.'
                     PERFORM  4405-TRATAR-ENTER-FASE-3

               WHEN  '9'
                     PERFORM  4410-TRATAR-PF9-FASE-3

               WHEN  'F'
                     PERFORM  4455-TRATAR-PF11-FASE-3

               WHEN  OTHER
                     MOVE  WRK-MSG02   TO  INP-MENSAG-T957

           END-EVALUATE.

      *----------------------------------------------------------------*
       4400-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4405-TRATAR-ENTER-FASE-3        SECTION.
      *----------------------------------------------------------------*

           PERFORM  4415-VERIFICAR-STATUS-MANUT

           EVALUATE  CSIT-ALT-REG OF RUCAB015

           WHEN  1
                 MOVE  WRK-MSG13       TO  INP-MENSAG-T957

           WHEN  2
                 MOVE  WRK-MSG14       TO  INP-MENSAG-T957

           WHEN  3
                 MOVE  WRK-MSG15       TO  INP-MENSAG-T957

           WHEN  4
                 MOVE  WRK-MSG16       TO  INP-MENSAG-T957

           WHEN  5
                 MOVE  WRK-MSG17       TO  INP-MENSAG-T957

           WHEN  6
                 MOVE  WRK-MSG18       TO  INP-MENSAG-T957

           WHEN  7
                 MOVE  WRK-MSG19       TO  INP-MENSAG-T957

           END-EVALUATE.

      *----------------------------------------------------------------*
       4405-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4410-TRATAR-PF9-FASE-3          SECTION.
      *----------------------------------------------------------------*

           PERFORM  4415-VERIFICAR-STATUS-MANUT

DPS10      PERFORM  4135-OBTER-DATA-HORA

           EVALUATE  CSIT-ALT-REG OF RUCAB015

           WHEN  1
           WHEN  2
                 PERFORM  4420-EFETUAR-CANCELAMENTO
                 MOVE  WRK-MSG20       TO  INP-MENSAG-T957

           WHEN  3
           WHEN  4
DPS10*** PALIATIVO PARA CANCELAR ENVIO DE CONTRATO QUE
DPS10*** FICOU PRESO  NO TMNF
DPS10            IF  WRK-DT-CORRENTE GREATER WRK-DT-INCL-REG
WI1021               PERFORM  4420-EFETUAR-CANCELAMENTO
WI1021               MOVE  WRK-MSG20   TO  INP-MENSAG-T957
WI1021           ELSE
                     MOVE  WRK-MSG21   TO  INP-MENSAG-T957
WI1021           END-IF

DPS11*** PALIATIVO PARA CANCELAR ENVIO DE CONTRATO 9192362 QUE
DPS11*** FICOU PRESO  NO TMNF
DPS11       IF  (INP-CHAVE-T957-N EQUAL 9192362
.           AND  WRK-TIMESTAMP    EQUAL '2024-08-27-10.01.11.892709')
.                    PERFORM  4420-EFETUAR-CANCELAMENTO
.                    MOVE  WRK-MSG20   TO  INP-MENSAG-T957
DPS11       END-IF

           WHEN  5
                 MOVE  WRK-MSG22       TO  INP-MENSAG-T957

           WHEN  6
                 MOVE  WRK-MSG23       TO  INP-MENSAG-T957

           WHEN  7
                 MOVE  WRK-MSG24       TO  INP-MENSAG-T957

           END-EVALUATE.

      *----------------------------------------------------------------*
       4410-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4415-VERIFICAR-STATUS-MANUT     SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-CHAVE-T957-N      TO  CCONTR           OF RUCAB015
           MOVE  INP-ANO-CTRLNAV-T957  TO  WRK-ANO-TIMES
DPS10                                      WRK-ANO-INCL-REG
           MOVE  INP-MES-CTRLNAV-T957  TO  WRK-MES-TIMES
DPS10                                      WRK-MES-INCL-REG
           MOVE  INP-DIA-CTRLNAV-T957  TO  WRK-DIA-TIMES
DPS10                                      WRK-DIA-INCL-REG
           MOVE  INP-HOR-CTRLNAV-T957  TO  WRK-HOR-TIMES
           MOVE  INP-MIN-CTRLNAV-T957  TO  WRK-MIN-TIMES
           MOVE  INP-SEG-CTRLNAV-T957  TO  WRK-SEG-TIMES
           MOVE  INP-MIL-CTRLNAV-T957  TO  WRK-MIL-TIMES
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB015

           EXEC SQL
                SELECT
                        CCONTR                        ,
                        HINCL_REG                     ,
                        CSIT_ALT_REG
                  INTO
                       :RUCAB015.CCONTR               ,
                       :RUCAB015.HINCL-REG            ,
                       :RUCAB015.CSIT-ALT-REG
                  FROM  DB2PRD.TMANUT_CONTR_RECOR
                 WHERE  CCONTR             = :RUCAB015.CCONTR
                   AND  HINCL_REG          = :RUCAB015.HINCL-REG
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CONTR_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0005'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4415-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4420-EFETUAR-CANCELAMENTO       SECTION.
      *----------------------------------------------------------------*

           PERFORM  4425-UPDATE-RUCAB015

           PERFORM  4430-ATUALIZAR-LOG.

      *----------------------------------------------------------------*
       4420-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4425-UPDATE-RUCAB015            SECTION.
      *----------------------------------------------------------------*

           MOVE  7                     TO  CSIT-ALT-REG     OF RUCAB015

           EXEC SQL
             UPDATE DB2PRD.TMANUT_CONTR_RECOR

                SET CSIT_ALT_REG       = :RUCAB015.CSIT-ALT-REG

             WHERE  CCONTR             = :RUCAB015.CCONTR
               AND  HINCL_REG          = :RUCAB015.HINCL-REG
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CONTR_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0006'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4425-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4430-ATUALIZAR-LOG              SECTION.
      *----------------------------------------------------------------*

           PERFORM  4135-OBTER-DATA-HORA
           PERFORM  4435-OPEN-CSR03-RUCAB014
           PERFORM  4440-FETCH-CSR03-RUCAB014

           PERFORM
             UNTIL  SQLCODE         EQUAL  +100

                    PERFORM  4450-UPDATE-RUCAB014
                    PERFORM  4440-FETCH-CSR03-RUCAB014
           END-PERFORM

           PERFORM  4445-CLOSE-CSR03-RUCAB014.

      *----------------------------------------------------------------*
       4430-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4435-OPEN-CSR03-RUCAB014        SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-CHAVE-T957-N      TO  CCONTR           OF RUCAB014
           MOVE  INP-ANO-CTRLNAV-T957  TO  WRK-ANO-TIMES
           MOVE  INP-MES-CTRLNAV-T957  TO  WRK-MES-TIMES
           MOVE  INP-DIA-CTRLNAV-T957  TO  WRK-DIA-TIMES
           MOVE  INP-HOR-CTRLNAV-T957  TO  WRK-HOR-TIMES
           MOVE  INP-MIN-CTRLNAV-T957  TO  WRK-MIN-TIMES
           MOVE  INP-SEG-CTRLNAV-T957  TO  WRK-SEG-TIMES
           MOVE  INP-MIL-CTRLNAV-T957  TO  WRK-MIL-TIMES
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB014

           EXEC SQL
                OPEN CSR03-RUCAB014
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TLOG_MANUT_CONTR' TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0007'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4435-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4440-FETCH-CSR03-RUCAB014       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR03-RUCAB014
                 INTO  :RUCAB014.CCONTR            ,
                       :RUCAB014.HINCL-REG         ,
                       :RUCAB014.NMANUT-EFETU      ,
                       :RUCAB014.RCPO-MANUT-RECOR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TLOG_MANUT_CONTR' TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0008'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4440-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4445-CLOSE-CSR03-RUCAB014       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR03-RUCAB014
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TLOG_MANUT_CONTR' TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0009'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4445-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4450-UPDATE-RUCAB014            SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-DIA-ATUAL         TO  WRK-DIA-DB2
           MOVE  '.'                   TO  WRK-PT1-DB2
           MOVE  WRK-MES-ATUAL         TO  WRK-MES-DB2
           MOVE  '.'                   TO  WRK-PT2-DB2
           MOVE  WRK-ANO-ATUAL         TO  WRK-ANO-DB2
           MOVE  WRK-DATA-DB2          TO  DMANUT           OF RUCAB014

           MOVE  WRK-COD-USER-N        TO  CFUNC-BDSCO      OF RUCAB014
           MOVE  74                    TO  RCPO-MANUT-RECOR-LEN
                                                            OF RUCAB014
           MOVE  ' - CANCELADO'        TO  RCPO-MANUT-RECOR-TEXT
                                                    OF RUCAB014 (63:12)

           EXEC SQL
             UPDATE DB2PRD.TLOG_MANUT_CONTR

                SET RCPO_MANUT_RECOR   = :RUCAB014.RCPO-MANUT-RECOR,
                    DMANUT             = :RUCAB014.DMANUT          ,
                    CFUNC_BDSCO        = :RUCAB014.CFUNC-BDSCO

             WHERE  CCONTR             = :RUCAB014.CCONTR
               AND  HINCL_REG          = :RUCAB014.HINCL-REG
               AND  NMANUT_EFETU       = :RUCAB014.NMANUT-EFETU
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TLOG_MANUT_CONTR' TO  ERR-DBD-TAB
               MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0010'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4450-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4455-TRATAR-PF11-FASE-3         SECTION.
      *----------------------------------------------------------------*

           PERFORM  4415-VERIFICAR-STATUS-MANUT

           EVALUATE  CSIT-ALT-REG OF RUCAB015

           WHEN  1
           WHEN  2
                 PERFORM  4215-CHAMAR-RUCA8812

                 IF  WRK-8812-COD-RETORNO  EQUAL  ZEROS
                     MOVE  WRK-MSG25   TO  INP-MENSAG-T957
                 END-IF

           WHEN  3
           WHEN  4
                 MOVE  WRK-MSG21       TO  INP-MENSAG-T957

           WHEN  5
                 MOVE  WRK-MSG22       TO  INP-MENSAG-T957

           WHEN  6
                 MOVE  WRK-MSG23       TO  INP-MENSAG-T957

           WHEN  7
                 MOVE  WRK-MSG24       TO  INP-MENSAG-T957

           END-EVALUATE.

      *----------------------------------------------------------------*
       4455-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4500-SELECT-RUECB048            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                SELECT
                        CTERM                       ,
                        CIDTFD_MDULO                ,
                        CSEQ_CTRL_MDULO             ,
                        WRESUL_CONS_MDULO
                  INTO
                        :RUECB048.CTERM             ,
                        :RUECB048.CIDTFD-MDULO      ,
                        :RUECB048.CSEQ-CTRL-MDULO   ,
                        :RUECB048.WRESUL-CONS-MDULO
                          :WRK-WRESUL-CONS-MDULO-NULL
                  FROM  DB2PRD.TAPOIO_UTILZ_MDULO
                 WHERE  CTERM              = :RUECB048.CTERM
                   AND  CIDTFD_MDULO       = :RUECB048.CIDTFD-MDULO
                   AND  CSEQ_CTRL_MDULO    = :RUECB048.CSEQ-CTRL-MDULO
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0011'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               IF  WRK-WRESUL-CONS-MDULO-NULL  EQUAL  -1
                   MOVE  LENGTH OF  WRK-DADOS-NAV-T957
                                       TO  WRESUL-CONS-MDULO-LEN
                                                            OF RUECB048
                   MOVE  SPACES        TO  WRESUL-CONS-MDULO-TEXT
                                                            OF RUECB048
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       4500-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4505-EXCLUIR-DADOS-NAV          SECTION.
      *----------------------------------------------------------------*

           MOVE  957                   TO  CIDTFD-MDULO    OF RUECB048
           MOVE  WRK-CSEQ-CTRL-MDULO   TO  CSEQ-CTRL-MDULO OF RUECB048

           PERFORM  4510-OPEN-CSR01-RUECB048
           PERFORM  4515-FETCH-CSR01-RUECB048

           PERFORM
             UNTIL  SQLCODE   EQUAL  +100

             PERFORM  4525-DELETE-RUECB048
             PERFORM  4515-FETCH-CSR01-RUECB048
           END-PERFORM.

           PERFORM  4520-CLOSE-CSR01-RUECB048.

      *----------------------------------------------------------------*
       4505-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4510-OPEN-CSR01-RUECB048        SECTION.
      *----------------------------------------------------------------*

           MOVE  LNK-IO-LTERM          TO  CTERM           OF RUECB048

           EXEC SQL
                OPEN CSR01-RUECB048
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0012'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4510-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4515-FETCH-CSR01-RUECB048       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR01-RUECB048
                 INTO  :RUECB048.CTERM            ,
                       :RUECB048.CIDTFD-MDULO     ,
                       :RUECB048.CSEQ-CTRL-MDULO  ,
                       :RUECB048.WRESUL-CONS-MDULO
                         :WRK-WRESUL-CONS-MDULO-NULL
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0013'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
              IF  WRK-WRESUL-CONS-MDULO-NULL  EQUAL  -1
                  MOVE  ZEROS          TO  WRESUL-CONS-MDULO-LEN
                  MOVE  SPACES         TO  WRESUL-CONS-MDULO-TEXT
              END-IF

              MOVE  WRESUL-CONS-MDULO-TEXT(1: WRESUL-CONS-MDULO-LEN)
                                       TO  WRK-DADOS-NAV-T957
           ELSE
               MOVE  SPACES            TO  WRK-DADOS-NAV-T957
           END-IF.

      *----------------------------------------------------------------*
       4515-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4520-CLOSE-CSR01-RUECB048       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUECB048
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0014'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4520-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4525-DELETE-RUECB048            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                DELETE
                  FROM  DB2PRD.TAPOIO_UTILZ_MDULO
                 WHERE  CTERM           = :RUECB048.CTERM
                   AND  CIDTFD_MDULO    = :RUECB048.CIDTFD-MDULO
                   AND  CSEQ_CTRL_MDULO = :RUECB048.CSEQ-CTRL-MDULO
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO'
                                       TO  ERR-DBD-TAB
               MOVE 'DELETE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0015'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4525-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4530-INSERT-RUECB048            SECTION.
      *----------------------------------------------------------------*

           MOVE  LENGTH OF WRK-DADOS-NAV-T957
                                       TO  WRESUL-CONS-MDULO-LEN
                                                          OF  RUECB048
           MOVE  WRK-DADOS-NAV-T957    TO  WRESUL-CONS-MDULO-TEXT
                                                          OF  RUECB048
           MOVE  ZEROS                 TO  WRK-WRESUL-CONS-MDULO-NULL

           EXEC SQL
                INSERT  INTO DB2PRD.TAPOIO_UTILZ_MDULO
                        (CTERM                         ,
                         CIDTFD_MDULO                  ,
                         CSEQ_CTRL_MDULO               ,
                         WRESUL_CONS_MDULO             )
                VALUES
                       (:RUECB048.CTERM                ,
                        :RUECB048.CIDTFD-MDULO         ,
                        :RUECB048.CSEQ-CTRL-MDULO      ,
                        :RUECB048.WRESUL-CONS-MDULO
                           :WRK-WRESUL-CONS-MDULO-NULL )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOIO_UTILZ_MDULO'
                                       TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0016'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4530-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE  INP-SENHAS-T957       TO  OUT-SENHAS-T957
           MOVE  INP-COMANDO-T957      TO  OUT-COMANDO-T957
           MOVE  INP-TRANSA-T957       TO  OUT-TRANSA-T957
           MOVE  INP-CPONAV-T957       TO  OUT-CPONAV-T957
           MOVE  INP-ATRIB-T957        TO  OUT-ATRIB-T957
           MOVE  INP-MENSAG-T957       TO  OUT-MENSAG-T957

      *----------------------------------------------------------------*
      * DEVOLVER OCORRENCIAS                                           *
      *----------------------------------------------------------------*

           PERFORM
           VARYING  IND-LIN          FROM  1 BY 1
             UNTIL  IND-LIN       GREATER  30
              MOVE  INP-LBANO-T957  (IND-LIN)
                                       TO  OUT-LBANO-T957      (IND-LIN)
              MOVE  INP-LBMES-T957  (IND-LIN)
                                       TO  OUT-LBMES-T957      (IND-LIN)
              MOVE  INP-LBDIA-T957  (IND-LIN)
                                       TO  OUT-LBDIA-T957      (IND-LIN)

              IF  INP-VALOR-T957(IND-LIN)      IS     NUMERIC  AND
                  INP-ATRIB-VALOR(IND-LIN) NOT EQUAL  9
                  MOVE  INP-VALOR-T957-N(IND-LIN)
                                          TO  WRK-N13V99-SAI-ED
                  MOVE  WRK-VALOR-SAI     TO  OUT-VALOR-T957   (IND-LIN)
              ELSE
                  MOVE  INP-VALOR-T957(IND-LIN)
                                          TO  OUT-VALOR-T957   (IND-LIN)
              END-IF

      ******************************************************************
      * ATRIBUTOS:                                                     *
      *                                                                *
      * 1 | NUMERICO | NORMAL   | DESPROTEGIDO |                       *
      * 2 | NUMERICO | NORMAL   | DESPROTEGIDO | POSICIONAMENTO CURSOR *
      * 3 | NUMERICO | BRILHANTE| DESPROTEGIDO | POSICIONAMENTO CURSOR *
      * 4 | NUMERICO | ALFA     | PROTEGIDO    |                       *
      ******************************************************************

              EVALUATE  INP-ATRIB-LBDATA(IND-LIN)
                 WHEN  1
                       MOVE  WRK-209   TO  OUT-LBDIA-ATTR-T957(IND-LIN)
                                           OUT-LBMES-ATTR-T957(IND-LIN)
                                           OUT-LBANO-ATTR-T957(IND-LIN)
                 WHEN  2
                       MOVE  WRK-49361 TO  OUT-LBDIA-ATTR-T957(IND-LIN)
                       MOVE  WRK-209   TO  OUT-LBMES-ATTR-T957(IND-LIN)
                                           OUT-LBANO-ATTR-T957(IND-LIN)
                 WHEN  3
                       MOVE  WRK-49369 TO  OUT-LBDIA-ATTR-T957(IND-LIN)
                       MOVE  WRK-217   TO  OUT-LBMES-ATTR-T957(IND-LIN)
                                           OUT-LBANO-ATTR-T957(IND-LIN)
                 WHEN  4
                       MOVE  WRK-225   TO  OUT-LBDIA-ATTR-T957(IND-LIN)
                                           OUT-LBMES-ATTR-T957(IND-LIN)
                                           OUT-LBANO-ATTR-T957(IND-LIN)
              END-EVALUATE

              EVALUATE  INP-ATRIB-VALOR(IND-LIN)
                 WHEN  1
                       MOVE  WRK-209   TO  OUT-VALOR-ATTR-T957(IND-LIN)

                 WHEN  2
                       MOVE  WRK-49361 TO  OUT-VALOR-ATTR-T957(IND-LIN)

                 WHEN  3
                       MOVE  WRK-49369 TO  OUT-VALOR-ATTR-T957(IND-LIN)

                 WHEN  4
                       MOVE  WRK-225   TO  OUT-VALOR-ATTR-T957(IND-LIN)

                 WHEN  9
                       MOVE  WRK-49369 TO  OUT-VALOR-ATTR-T957(IND-LIN)

              END-EVALUATE
           END-PERFORM.

      *----------------------------------------------------------------*
       5000-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6000-BRAD0660-RUCAT957          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF OUTPUT-T957  TO  OUT-LL-T957
                                           660-LL-MENSAGEM-T957
           MOVE LENGTH OF 660-RUCAT957 TO  660-LL-AREA-T957

           CALL  'BRAD0660'         USING  OUTPUT-T957
                                           660-RUCAT957

           IF  RETURN-CODE      NOT EQUAL  ZEROS
               MOVE  'APL'             TO  ERR-TIPO-ACESSO
               MOVE  RETURN-CODE       TO  WRK-RETURN-CODE
               MOVE  17                TO  WRK-LOCAL-ERRO
               MOVE  WRK-ERRO-BRAD0660 TO  ERR-TEXTO
               PERFORM  9999-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO  WRK-FUNCAO
           MOVE  OUTPUT-T957           TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       6000-99-FIM.    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE  'SENHAS02'            TO  ERR-PGM
           MOVE  WRK-COD-DEPTO         TO  ERR-COD-DEPTO
           MOVE  WRK-COD-USER          TO  ERR-COD-USER
           MOVE  'RUCA6957'            TO  ERR-MODULO

           IF  ERR-TIPO-ACESSO      EQUAL  'APL'
               CALL 'BRAD7100'      USING  LNK-IO-PCB
                                           ERRO-AREA
                                           LNK-ALT-PCB
           ELSE
               CALL 'BRAD7100'      USING  LNK-IO-PCB
                                           ERRO-AREA
                                           LNK-ALT-PCB
                                           SQLCA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.    EXIT.
      *----------------------------------------------------------------*
