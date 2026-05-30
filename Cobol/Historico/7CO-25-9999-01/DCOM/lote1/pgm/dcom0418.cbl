      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0418.
       AUTHOR.     MARCELO MORINA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0418                                    *
      *    PROGRAMADOR.:   MARCELO MORINA MARQUES                      *
      *    ANALISTA....:   MARCELO MORINA MARQUES                      *
      *    DATA........:   08/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CANCELAR LIBERACAO CONTRATO,                *
      *                    CONTRATOS POR CLIENTE.                      *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDRTM - CANCELAR LIBERACAO CONTRATO,                     *
      *               CONTRATOS POR CLIENTE.                           *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSG - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *    DCOM6416 - BUSCA NOME DO CLIENTE                            *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0413           CHAMA: DCOM0419            *
      *================================================================*
WIP001*                     A L T E R A C A O                          *
WIP001*----------------------------------------------------------------*
WIP001*  ANALISTA......:   FERNANDO LUIZ DE SANTI                      *
WIP001*  ANALISTA DS...:   PAULO ROBERTO        - GRUPO 39             *
WIP001*  DATA..........:   25/02/2021                                  *
WIP001*----------------------------------------------------------------*
WIP001*  OBJETIVO......:   VALIDAR SE CONTRATO LIMITE ESTA LIGADO A    *
WIP001*    ALGUMA OPERACAO, POR CAUSA DO OPTIN.                        *
WIP001*----------------------------------------------------------------*
WIP001*  MODULO........:                                               *
WIP001*  DCOM5124 - VER SE CONTRATO LIMITE TEM OPERACAO.               *
WIP001*================================================================*
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0418 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-225                     PIC S9(008) COMP    VALUE +225.
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-241                     PIC S9(008) COMP    VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS CURSOR - NUM  ---*
       01  WRK-49345                   PIC S9(008) COMP    VALUE +49345.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS CURSOR - NUM  ---*
       01  WRK-49361                   PIC S9(008) COMP    VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS CURSOR - NUM  ---*
       01  WRK-49369                   PIC S9(008) COMP    VALUE +49369.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-IND                 PIC  9(002)         VALUE ZEROS.
WIP001     05  IND-1                   PIC  9(002) COMP-3  VALUE ZEROS.
           05  WRK-QTD-C               PIC  9(001)         VALUE ZEROS.
           05  WRK-QTD-D               PIC  9(001)         VALUE ZEROS.
           05  WRK-QTD-SPACES          PIC  9(001)         VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.
           05  WCT-SELEC               PIC S9(09) COMP     VALUE ZEROS.
           05  WRK-EDT-VAL             PIC ZZ.ZZZ.ZZZ.ZZ9,99.
           05  WRK-CANCELOU-OK         PIC X(01)           VALUE SPACES.

           05  WRK-MENSA-PF7           PIC  X(079) VALUE
               'PF7 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA VOLTAR'.

           05  WRK-MENSA-PF8           PIC  X(079) VALUE
               'PF8 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA AVANCAR'.

       01  WRK-AGENCIA-AUX.
           05  WRK-AGENCIA-NUM         PIC  9(005)         VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)         VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)         VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)         VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(004)         VALUE SPACES.
           05  WRK-TELA                PIC  X(008)         VALUE SPACES.
           05  WRK-VERSAO              PIC  X(006)         VALUE
               'VRS002'.
           05  WRK-COD-USER            PIC  X(007)         VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).
           05  WRK-COD-DEPTO.
               10  FILLER              PIC  X(002)         VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)         VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)         VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01 WRK-ERRO-BRAD0660.
           05 FILLER                   PIC  X(045)         VALUE
              '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05 FILLER                   PIC  X(012)         VALUE
              '- RET.COD ='.
           05 WRK-RETURN-CODE          PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(011)         VALUE
              ' - LOCAL ='.
           05 WRK-LOCAL-ERRO           PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDRTM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-DRTM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-DRTM        PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-DRTM     PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-DRTM    PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-DRTM.
               10  WRK-INP-FASE-DRTM-X.
                   15  WRK-INP-FASE-DRTM-N
                                        PIC  9(001) VALUE ZEROS.
               10  WRK-INP-TIPOOPC-DRTM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-INP-TMSTAMP-DRTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-INP-FIMAMOS-DRTM PIC  X(01)          VALUE SPACE.
               10  WRK-INP-PAGINA-DRTM-X.
                   15  WRK-INP-PAGINA-DRTM-N
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-NMCLIE-DRTM  PIC  X(040) VALUE SPACES.
               10  WRK-INP-CNPJX-DRTM   PIC  X(09)  VALUE SPACES.
               10  WRK-INP-CNPJCPF-DRTM.
FS2511*            15  WRK-INP-NUMERO-DRTM
FS2511*                                 PIC  999.999.999.
FS2511             15  WRK-INP-NUMERO-DRTM
FS2511                                  PIC  X(11)  VALUE SPACES.
                   15  WRK-INP-BARRA-1-DRTM
                                        PIC  X(01)  VALUE SPACES.
FS2511*            15  WRK-INP-FILIAL-DRTM
FS2511*                                     PIC  9(04)  VALUE ZEROS.
FS2511             15  WRK-INP-FILIAL-DRTM
FS2511                                      PIC  X(04)  VALUE SPACES.
                   15  WRK-INP-HIFEN-2-DRTM
                                        PIC  X(01)  VALUE SPACES.
                   15  WRK-INP-CONTROLE-DRTM
                                        PIC  9(02)  VALUE ZEROS.
               10  WRK-INP-BANCOX-DRTM  PIC  X(06)  VALUE SPACES.
               10  WRK-INP-BANCO-DRTM-X.
                   15  WRK-INP-BANCO-DRTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-AGENX-DRTM   PIC  X(08)  VALUE SPACES.
               10  WRK-INP-AGENCIA-GR-DRTM.
                   15  WRK-INP-CODAGE-DRTM
                                        PIC  9(005) VALUE ZEROS.
                   15  WRK-INP-HIFEN-AG-DRTM
                                        PIC  X(001) VALUE SPACES.
                   15  WRK-INP-DESCAGEN-DRTM
                                        PIC  X(023) VALUE SPACES.
               10  WRK-INP-CONTAX-DRTM  PIC  X(09)  VALUE SPACES.
               10  WRK-INP-CONTA-DRTM-X.
                   15  WRK-INP-CONTA-DRTM
                                        PIC  9(013) VALUE ZEROS.
               10  WRK-INP-TIPO-PESQ-DRTM PIC  9(001) VALUE ZEROS.
               10  WRK-INP-TABELA-DRTM    OCCURS  05  TIMES.
                   15  WRK-INP-SELEC-DRTM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-CONTR-DRTM-X.
                       20  WRK-INP-CONTR-DRTM PIC  9(009) VALUE ZEROS.
                   15  WRK-INP-NOME-DRTM      PIC  X(030) VALUE SPACES.
                   15  WRK-INP-DTINIC-DRTM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTVENCT-DRTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-LIMITE-DRTM    PIC  X(017) VALUE SPACES.
                   15  WRK-INP-SITUAC-DRTM    PIC  X(020) VALUE SPACES.
           05  WRK-INP-MENSA-DRTM       PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDRTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-DRTM.
           05  WRK-OUT-LL-DRTM          PIC S9(004) COMP VALUE +0852.
           05  WRK-OUT-ZZ-DRTM          PIC  9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DRTM      PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-DRTM     PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-DRTM.
               10  WRK-OUT-FASE-DRTM-X.
                   15  WRK-OUT-FASE-DRTM-N
                                        PIC  9(001) VALUE ZEROS.
               10  WRK-OUT-TIPOOPC-DRTM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-TMSTAMP-DRTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-FIMAMOS-DRTM PIC  X(01)          VALUE SPACE.
               10  WRK-OUT-PAGINA-DRTM-X.
                   15  WRK-OUT-PAGINA-DRTM-N
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-NMCLIE-DRTM  PIC  X(040) VALUE SPACES.
               10  WRK-OUT-CNPJX-DRTM   PIC  X(09)  VALUE SPACES.
               10  WRK-OUT-CNPJCPF-DRTM.
FS2511*            15  WRK-OUT-NUMERO-DRTM
FS2511*                                 PIC  999.999.999.
FS2511             15  WRK-OUT-NUMERO-DRTM
FS2511                                  PIC  X(11) VALUE SPACES.
                   15  WRK-OUT-BARRA-1-DRTM
                                        PIC  X(001) VALUE SPACES.
FS2511*            15  WRK-OUT-FILIAL-DRTM
FS2511*                                 PIC  9(004) VALUE ZEROS.
FS2511             15  WRK-OUT-FILIAL-DRTM
FS2511                                  PIC  X(004) VALUE SPACES.
                   15  WRK-OUT-HIFEN-2-DRTM
                                        PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-CONTROLE-DRTM
                                        PIC  9(002) VALUE ZEROS.
               10  WRK-OUT-BANCOX-DRTM  PIC  X(06)  VALUE SPACES.
               10  WRK-OUT-BANCO-DRTM-X.
                   15  WRK-OUT-BANCO-DRTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-AGENX-DRTM   PIC  X(08)  VALUE SPACES.
               10  WRK-OUT-AGENCIA-GR-DRTM.
                   15  WRK-OUT-CODAGE-DRTM-X.
                       20  WRK-OUT-CODAGE-DRTM
                                        PIC  9(005) VALUE ZEROS.
                   15  WRK-OUT-HIFEN-AG-DRTM
                                        PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-DESCAGEN-DRTM
                                        PIC  X(023) VALUE SPACES.
               10  WRK-OUT-CONTAX-DRTM  PIC  X(09)  VALUE SPACES.
               10  WRK-OUT-CONTA-DRTM-X.
                   15  WRK-OUT-CONTA-DRTM
                                        PIC  9(013) VALUE ZEROS.
               10  WRK-OUT-TIPO-PESQ-DRTM PIC  9(001) VALUE ZEROS.
               10  WRK-OUT-TABELA-DRTM.
                   15  WRK-OUT-LINHAS-DRTM OCCURS 05 TIMES.
                       20  WRK-OUT-SELEC-ATTR-DRTM
                                       PIC S9(004) COMP VALUE ZEROS.
                       20  WRK-OUT-SELEC-DRTM
                                               PIC  X(001) VALUE SPACES.
                       20  WRK-OUT-CONTR-DRTM-X.
                           25  WRK-OUT-CONTR-DRTM
                                               PIC  9(009) VALUE ZEROS.
                       20  WRK-OUT-NOME-DRTM
                                               PIC  X(030) VALUE SPACES.
                       20  WRK-OUT-DTINIC-DRTM
                                               PIC  X(010) VALUE SPACES.
                       20  WRK-OUT-DTVENCT-DRTM
                                               PIC  X(010) VALUE SPACES.
                       20  WRK-OUT-LIMITE-DRTM-X.
                           25  WRK-OUT-LIMITE-DRTM
                                               PIC  X(017) VALUE SPACES.
                       20  WRK-OUT-SITUAC-DRTM
                                               PIC  X(020) VALUE SPACES.
               10  WRK-OUT-MENSA-DRTM   PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDRTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMDRTM.
           05  660-LL-AREA-DRTM        PIC  9(04)  COMP  VALUE 0110.
           05  660-LL-MENSAGEM-DRTM    PIC  9(04)  COMP  VALUE 0852.
           05  660-SENHAS-DRTM         PIC  9(04)  COMP  VALUE 0037.
           05  660-COMANDO-DRTM        PIC  9(04)  COMP  VALUE 0068.
           05  660-FASE-DRTM           PIC  9(04)  COMP  VALUE 0001.
           05  660-TIPOOPC-DRTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-TMSTAMP-DRTM        PIC  9(04)  COMP  VALUE 0026.
           05  660-FIMAMOS-DRTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-PAGINA-DRTM         PIC  9(04)  COMP  VALUE 0003.
           05  660-NMCLIE-DRTM         PIC  9(04)  COMP  VALUE 0040.
           05  660-CNPJX-DRTM          PIC  9(04)  COMP  VALUE 0009.
           05  660-CNPJCPF-DRTM        PIC  9(04)  COMP  VALUE 0019.
           05  660-BANCOX-DRTM         PIC  9(04)  COMP  VALUE 0006.
           05  660-BANCO-DRTM          PIC  9(04)  COMP  VALUE 0003.
           05  660-AGENX-DRTM          PIC  9(04)  COMP  VALUE 0008.
           05  660-AGENCIA-DRTM        PIC  9(04)  COMP  VALUE 0029.
           05  660-CONTAX-DRTM         PIC  9(04)  COMP  VALUE 0009.
           05  660-CONTA-DRTM          PIC  9(04)  COMP  VALUE 0013.
           05  660-TPPESQ-DRTM         PIC  9(04)  COMP  VALUE 0001.
           05  660-TABELA-OCCURS       OCCURS   5  TIMES.
               10  660-SELEC-DRTM      PIC  9(04)  COMP  VALUE 2003.
               10  660-CONTR-DRTM      PIC  9(04)  COMP  VALUE 0009.
               10  660-NOME-DRTM       PIC  9(04)  COMP  VALUE 0030.
               10  660-DTINIC-DRTM     PIC  9(04)  COMP  VALUE 0010.
               10  660-DTVENCT-DRTM    PIC  9(04)  COMP  VALUE 0010.
               10  660-LIMITE-DRTM     PIC  9(04)  COMP  VALUE 0017.
               10  660-SITUAC-DRTM     PIC  9(04)  COMP  VALUE 0020.
           05  660-MENSA-DRTM          PIC  9(04)  COMP  VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE RECEBIMENTO E RETORNO DO DCOM0413 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-413.
           05  COMU-LL-413            PIC S9(004) COMP    VALUE +193.
           05  COMU-ZZ-413            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-413.
               10  COMU-TRANSACAO-413 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(006)         VALUE SPACES.
               10  COMU-PFK-413       PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-413        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-413       PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS-413.
               10  COMU-OPCAO-413    PIC  9(001)          VALUE ZEROS.
               10  COMU-TMSTAMP-413  PIC  X(026)          VALUE SPACES.
               10  COMU-CODAGE-413   PIC  9(005)          VALUE ZEROS.
               10  COMU-CONTA-413    PIC  9(013)          VALUE ZEROS.
               10  COMU-NUMERO-413   PIC  9(009)          VALUE ZEROS.
FS2511*        10  COMU-FILIAL-413   PIC  9(004)          VALUE ZEROS.
FS2511         10  COMU-FILIAL-413   PIC  X(04)           VALUE SPACES.
               10  COMU-CONTROLE-413 PIC  9(002)          VALUE ZEROS.

       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP  VALUE +272.
           05  COMU-ZZ                 PIC S9(004) COMP  VALUE +0.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(006)       VALUE SPACES.
               10  COMU-PFK            PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)       VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-OPCAO          PIC  9(001)       VALUE ZEROS.
               10  COMU-TIMESTAMP      PIC  X(026)       VALUE SPACES.
               10  COMU-CONTRATO       PIC  9(009)       VALUE ZEROS.
               10  COMU-AGENCIA        PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA          PIC  9(013)       VALUE ZEROS.
               10  COMU-CPF-X.
                   15  COMU-CPF        PIC  9(009)       VALUE ZEROS.
FS2511*        10  COMU-FILIAL         PIC  9(004)       VALUE ZEROS.
FS2511         10  COMU-FILIAL         PIC  X(04)        VALUE SPACES.
               10  COMU-DIGITO         PIC  9(002)       VALUE ZEROS.
               10  COMU-MENSAG         PIC  X(079)       VALUE SPACES.

      *---------------------------------------------------------------
      *        *** AREA DE COMUNICACAO CHAMANDO DCOM0419
      *---------------------------------------------------------------

       01  COMU-AREA-419.
           05  COMU-LL-419            PIC S9(004) COMP    VALUE +276.
           05  COMU-ZZ-419            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-419.
               10  COMU-TRANSACAO-419 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(007)         VALUE SPACES.
           05  COMU-SENHAS-419        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-419       PIC  X(068)         VALUE SPACES.
           05  COMU-OPCAO-419         PIC  9(001)         VALUE ZEROS.
           05  COMU-TMSTAMP-419       PIC  X(026)         VALUE SPACES.
           05  COMU-CONTRATO-419      PIC  9(009)         VALUE ZEROS.
           05  COMU-CODAGE-419-X.
               10  COMU-CODAGE-419-N  PIC  9(005)         VALUE ZEROS.
           05  COMU-CONTA-419-X.
               10  COMU-CONTA-419-N   PIC  9(013)          VALUE ZEROS.
           05  COMU-NUMERO-419-X.
               10  COMU-NUMERO-419-N  PIC  9(009)          VALUE ZEROS.
           05  COMU-FILIAL-419-X.
FS2511*        10  COMU-FILIAL-419-N  PIC  9(004)          VALUE ZEROS.
FS2511         10  COMU-FILIAL-419-N  PIC  X(04)           VALUE SPACES.
           05  COMU-CONTROLE-419-X.
               10  COMU-CONTROLE-419-N PIC 9(002)          VALUE ZEROS.
           05  COMU-PAGINA-419         PIC 9(004)          VALUE ZEROS.
           05  COMU-MENSA-419          PIC X(079)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM5143 **'.
      *----------------------------------------------------------------*

       01  WRK-5143E-ENTRADA.
           03  WRK-5143E-COD-RETORNO             PIC  X(04).
           03  WRK-5143E-MSG-RETORNO             PIC  X(79).
           03  WRK-5143E-RESTART                 PIC  9(05).
           03  WRK-5143E-FLAG                    PIC  X(01).
           03  WRK-5143E-CFUNC-BDSCO             PIC  9(09).
           03  WRK-5143E-CTERM                   PIC  X(08).
           03  WRK-5143E-CCONTR-LIM-DESC         PIC  9(09).

       01  WRK-5143S-SAIDA.
           03  WRK-5143S-HEADER.
               05  WRK-5143S-COD-RETORNO         PIC  X(04).
               05  WRK-5143S-MSG-RETORNO         PIC  X(79).
               05  WRK-5143S-RESTART             PIC  9(05).
               05  WRK-5143S-FLAG                PIC  X(01).
               05  WRK-5143S-COUNT               PIC  9(09).
               05  WRK-5143S-TAMANHO-ERRO        PIC  9(01).
               05  WRK-5143S-CAMPOS-ERRO         PIC  9(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4111 **'.
      *----------------------------------------------------------------*
       01  WRK-4111E-ENTRADA.
           03  WRK-4111E-HEADER.
               05  WRK-4111E-COD-RETORNO             PIC  X(04).
               05  WRK-4111E-MSG-RETORNO             PIC  X(79).
               05  WRK-4111E-RESTART                 PIC  9(05).
               05  WRK-4111E-FLAG                    PIC  X(01).
               05  WRK-4111E-START-COUNT             PIC  X(01).
               05  WRK-4111E-CFUNC-BDSCO             PIC  9(09).
               05  WRK-4111E-CTERM                   PIC  X(08).
           03  WRK-4111E-DADOS.
               05  WRK-4111E-CAG-BCRIA               PIC  9(05).
               05  WRK-4111E-CCTA-BCRIA              PIC  9(13).
               05  WRK-4111E-FLAG-FILTRO             PIC  X(01).

       01  WRK-4111S-SAIDA.
           03  WRK-4111S-HEADER.
               05  WRK-4111S-COD-RETORNO            PIC  X(04).
               05  WRK-4111S-MSG-RETORNO            PIC  X(79).
               05  WRK-4111S-RESTART                PIC  9(05).
               05  WRK-4111S-FLAG                   PIC  X(01).
               05  WRK-4111S-QTDE-RETORNADA         PIC  9(03).
               05  WRK-4111S-COUNT                  PIC  9(09).
           03  WRK-4111S-DADOS.
               05  WRK-4111S-TABELA-SAIDA    OCCURS 20 TIMES.
                   10  WRK-4111S-CCONTR-LIM-DESC    PIC  9(09).
                   10  WRK-4111S-VERSAO-CCONTR-LIM  PIC  9(03).
FS2511*            10  WRK-4111S-CCNPJ-CPF          PIC  9(09).
FS2511             10  WRK-4111S-CCNPJ-CPF          PIC  X(09).
FS2511*            10  WRK-4111S-CFLIAL-CNPJ        PIC  9(04).
FS2511             10  WRK-4111S-CFLIAL-CNPJ        PIC  X(04).
                   10  WRK-4111S-CCTRL-CNPJ-CPF     PIC  9(02).
                   10  WRK-4111S-NM-CLIE            PIC  X(25).
                   10  WRK-4111S-DINIC-VGCIA-CONTR  PIC  X(10).
                   10  WRK-4111S-DVCTO-CONTR-LIM    PIC  X(10).
                   10  WRK-4111S-VCONTR-LIM         PIC  9(15)V99.
                   10  WRK-4111S-IRSUMO-SIT-DESC    PIC  X(15).
           03  WRK-4111S-CONSISTENCIA   REDEFINES   WRK-4111S-DADOS.
               05  WRK-4111S-TAMANHO-ERRO           PIC  9(01).
               05  WRK-4111S-TABELA-ERRO   OCCURS   2    TIMES.
                   10  WRK-4111S-CAMPOS-ERRO        PIC  9(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4112 **'.
      *----------------------------------------------------------------*
       01  WRK-4112E-ENTRADA.
           03  WRK-4112E-HEADER.
               05  WRK-4112E-COD-RETORNO             PIC  X(04).
               05  WRK-4112E-MSG-RETORNO             PIC  X(79).
               05  WRK-4112E-RESTART                 PIC  9(05).
               05  WRK-4112E-FLAG                    PIC  X(01).
               05  WRK-4112E-START-COUNT             PIC  X(01).
               05  WRK-4112E-CFUNC-BDSCO             PIC  9(09).
               05  WRK-4112E-CTERM                   PIC  X(08).
           03  WRK-4112E-DADOS.
FS2511*        05  WRK-4112E-CCNPJ-CPF               PIC  9(09).
FS2511         05  WRK-4112E-CCNPJ-CPF               PIC  X(09).
FS2511*        05  WRK-4112E-CFLIAL-CNPJ             PIC  9(04).
FS2511         05  WRK-4112E-CFLIAL-CNPJ             PIC  X(04).
               05  WRK-4112E-CCTRL-CNPJ-CPF          PIC  9(02).
               05  WRK-4112E-FLAG-FILTRO             PIC  X(01).

       01  WRK-4112S-SAIDA.
           03  WRK-4112S-HEADER.
               05  WRK-4112S-COD-RETORNO            PIC  X(04).
               05  WRK-4112S-MSG-RETORNO            PIC  X(79).
               05  WRK-4112S-RESTART                PIC  9(05).
               05  WRK-4112S-FLAG                   PIC  X(01).
               05  WRK-4112S-QTDE-RETORNADA         PIC  9(03).
               05  WRK-4112S-COUNT                  PIC  9(09).
           03  WRK-4112S-DADOS.
               05  WRK-4112S-TABELA-SAIDA    OCCURS 30 TIMES.
                   10  WRK-4112S-NM-CLIE            PIC  X(25).
                   10  WRK-4112S-CCONTR-LIM-DESC    PIC  9(09).
                   10  WRK-4112S-CVRSAO-CONTR-LIM   PIC  9(03).
                   10  WRK-4112S-CAG-BCRIA          PIC  9(05).
                   10  WRK-4112S-CCTA-BCRIA         PIC  9(13).
                   10  WRK-4112S-DINIC-VGCIA-CONTR  PIC  X(10).
                   10  WRK-4112S-DVCTO-CONTR-LIM    PIC  X(10).
                   10  WRK-4112S-VCONTR-LIM         PIC  9(15)V99.
                   10  WRK-4112S-IRSUMO-SIT-DESC    PIC  X(15).
           03  WRK-4112S-CONSISTENCIA   REDEFINES   WRK-4112S-DADOS.
               05  WRK-4112S-TAMANHO-ERRO           PIC  9(01).
               05  WRK-4112S-TABELA-ERRO   OCCURS   1    TIMES.
                   10  WRK-4112S-CAMPOS-ERRO        PIC  9(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM6416 **'.
      *----------------------------------------------------------------*

       01  WRK-6416E-ENTRADA.
           03  WRK-6416E-COD-RETORNO      PIC  X(004).
           03  WRK-6416E-MSG-RETORNO      PIC  X(079).
           03  WRK-6416E-RESTART          PIC  9(005).
           03  WRK-6416E-FLAG             PIC  X(001).
           03  WRK-6416E-CFUNC-BDSCO      PIC  9(009).
           03  WRK-6416E-CTERM            PIC  X(008).
           03  WRK-6416E-BANCO            PIC  9(003).
           03  WRK-6416E-DEPDC            PIC  9(005).

       01  WRK-6416S-SAIDA.
           03  WRK-6416S-HEADER.
               05  WRK-6416S-COD-RETORNO  PIC  X(004).
               05  WRK-6416S-MSG-RETORNO  PIC  X(079).
               05  WRK-6416S-RESTART      PIC  9(005).
               05  WRK-6416S-FLAG         PIC  X(001).
           03  WRK-6416S-DADOS.
               05  WRK-6416S-TABELA-SAIDA.
                   07  WRK-6416S-AG-DEPDC            PIC 9(05).
                   07  WRK-6416S-DG-DEPDC            PIC X(01).
                   07  WRK-6416S-NOME-DEPDC          PIC X(40).
                   07  WRK-6416S-END-DEPDC           PIC X(30).
                   07  WRK-6416S-DIRETORIA-REG       PIC 9(05).
                   07  WRK-6416S-DIRETORIA-NOME      PIC X(40).
                   07  WRK-6416S-NOME-BANCO          PIC X(40).
                   07  WRK-6416S-POLO-SERVICO        PIC 9(05).
                   07  WRK-6416S-MUNIC-DEPDC         PIC X(25).
                   07  WRK-6416S-UF                  PIC X(02).
                   07  WRK-6416S-CEP                 PIC 9(05).
                   07  WRK-6416S-CEP-COMPL           PIC 9(03).
                   07  WRK-6416S-INDIC-DEPDC-AT      PIC X(01).
                   07  WRK-6416S-NATUREZA-DEPDC      PIC X(01).
                   07  WRK-6416S-COD-DISTR-CORR      PIC 9(05).
                   07  WRK-6416S-DENCRR-DEPDC-BDSCO  PIC X(10).

WIP001*----------------------------------------------------------------*
WIP001 01  FILLER                      PIC  X(050)         VALUE
WIP001     '*** AREA PARA ACESSO AO DCOM5124 ***'.
WIP001*----------------------------------------------------------------*
WIP001
WIP001 01  WRK-DCOM5124-ENTRADA.
WIP001     05  WRK-5124E-COD-RETORNO    PIC  X(004)        VALUE SPACES.
WIP001     05  WRK-5124E-MSG-RETORNO    PIC  X(079)        VALUE SPACES.
WIP001     05  WRK-5124E-TRANSACAO      PIC  X(009)        VALUE SPACES.
WIP001     05  WRK-5124E-DADOS.
WIP001         10  WRK-5124E-CCONTR-LIM PIC  9(009)        VALUE ZEROS.
WIP001         10  WRK-5124E-CVRSAO-CON PIC  9(003)        VALUE ZEROS.
WIP001
WIP001 01  WRK-DCOM5124-SAIDA.
WIP001     05  WRK-5124S-COD-RETORNO    PIC  9(004)        VALUE ZEROS.
WIP001     05  WRK-5124S-MSG-RETORNO    PIC  X(079)        VALUE SPACES.
WIP001
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*
           COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM6408 ***'.
      *----------------------------------------------------------------*
           COPY  'I#DCOMX3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM11   ***'.
      *----------------------------------------------------------------*
           COPY  'I#DCOM11'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
           COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA BRAD0431  ***'.
      *----------------------------------------------------------------*
       01  AREA-BRAD0431.
           03  WRK-CONTA-0431          PIC  9(13) COMP-3.
           03  WRK-CONTA-0431-R        PIC  9(13)          VALUE ZEROS.
           03  WRK-DIGITO-0431         PIC  X(01)          VALUE SPACES.
           03  WRK-DIGITO-0431-R REDEFINES WRK-DIGITO-0431 PIC 9(01).
           03  WRK-TAMANHO-0431        PIC  9(02)          VALUE 7.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0418 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01 LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE          EQUAL  04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO  WRK-INP-DADOS-DRTM
                                           WRK-OUT-DADOS-DRTM
                                           WRK-OUT-SENHAS-DRTM
                                           WRK-OUT-COMANDO-DRTM
                                           WRK-INCONSIS.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROGRAMA A MENSAGEM                               *
      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMDRTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO    EQUAL  'DCOM0413'
               PERFORM 1100-RECEBER-CONTROLE-413
               MOVE  1             TO WRK-OUT-FASE-DRTM-N
               MOVE 'SELECIONE UM REGISTRO COM "D" OU VARIOS COM "C" E T
      -         'ECLE <ENTER>'     TO  WRK-OUT-MENSA-DRTM
           ELSE
             IF  WRK-TRANSACAO    EQUAL  'DCOM0419'
                 MOVE 'S'             TO WRK-CANCELOU-OK
                 PERFORM 1200-RECEBER-CONTROLE-419
                 MOVE  1              TO WRK-OUT-FASE-DRTM-N
               MOVE 'SELECIONE UM REGISTRO COM "D" OU VARIOS COM "C" E T
      -         'ECLE <ENTER>'        TO  WRK-OUT-MENSA-DRTM
                 IF  COMU-MENSA-419   NOT EQUAL  SPACES
                     MOVE COMU-MENSA-419  TO WRK-OUT-MENSA-DRTM
                 END-IF
             ELSE
                 PERFORM 1300-PROCESSAR-DCOMDRTM
             END-IF
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMDRTM'
               PERFORM 1700-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RECEBE O CONTROLE DO DCOM0413                     *
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE-413       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO  COMU-AREA-413.
           MOVE ZEROS                  TO  WRK-INP-PAGINA-DRTM-N
                                           WRK-OUT-PAGINA-DRTM-N.

           MOVE COMU-TMSTAMP-413       TO  WRK-INP-TMSTAMP-DRTM
                                           WRK-OUT-TMSTAMP-DRTM
           MOVE COMU-OPCAO-413         TO  WRK-INP-TIPOOPC-DRTM
                                           WRK-OUT-TIPOOPC-DRTM
           MOVE ZEROS                  TO  WRK-4111E-RESTART
                                           WRK-4112E-RESTART
           MOVE 1                      TO  WRK-OUT-PAGINA-DRTM-N

           IF COMU-CODAGE-413  IS NUMERIC AND
              COMU-CODAGE-413  IS GREATER ZEROS
              MOVE  1                  TO  WRK-INP-TIPO-PESQ-DRTM
                                           WRK-OUT-TIPO-PESQ-DRTM
              MOVE 237                 TO  WRK-INP-BANCO-DRTM
                                           WRK-OUT-BANCO-DRTM
              MOVE COMU-CODAGE-413     TO  WRK-INP-CODAGE-DRTM
                                           WRK-OUT-CODAGE-DRTM
                                           WRK-4111E-CAG-BCRIA
              MOVE '-'                 TO  WRK-INP-HIFEN-AG-DRTM
                                           WRK-OUT-HIFEN-AG-DRTM
              MOVE COMU-CONTA-413      TO  WRK-INP-CONTA-DRTM
                                           WRK-OUT-CONTA-DRTM
                                           WRK-4111E-CCTA-BCRIA

              PERFORM  1110-MONTAR-CABECALHO

              MOVE 1                   TO  WRK-INP-PAGINA-DRTM-N
              PERFORM 1500-ACESSAR-DCOM4111

           ELSE

              MOVE  2                  TO  WRK-INP-TIPO-PESQ-DRTM
                                           WRK-OUT-TIPO-PESQ-DRTM
              MOVE COMU-NUMERO-413     TO  WRK-INP-NUMERO-DRTM
                                           WRK-OUT-NUMERO-DRTM
                                           WRK-4112E-CCNPJ-CPF
              MOVE '/'                 TO  WRK-INP-BARRA-1-DRTM
                                           WRK-OUT-BARRA-1-DRTM
              MOVE COMU-FILIAL-413     TO  WRK-INP-FILIAL-DRTM
                                           WRK-OUT-FILIAL-DRTM
                                           WRK-4112E-CFLIAL-CNPJ
              MOVE '-'                 TO  WRK-INP-HIFEN-2-DRTM
                                           WRK-OUT-HIFEN-2-DRTM
              MOVE COMU-CONTROLE-413   TO  WRK-INP-CONTROLE-DRTM
                                           WRK-OUT-CONTROLE-DRTM
                                           WRK-4112E-CCTRL-CNPJ-CPF

              PERFORM  1110-MONTAR-CABECALHO

              PERFORM 1600-ACESSAR-DCOM4112
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA MONTA O CABECALHO POR AGENCIA E CONTA             *
      *----------------------------------------------------------------*
       1110-MONTAR-CABECALHO           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-TIPO-PESQ-DRTM    EQUAL  1
               PERFORM  1120-ACESSAR-DCOM6416
               PERFORM  1130-CALCULAR-DIGITO
           END-IF.

           PERFORM 1140-ACESSAR-DCOM6408.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA ACESSA O MODULO DCOM6416                          *
      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM6416           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-6416E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-6416E-CTERM
           MOVE 237                    TO  WRK-6416E-BANCO
           MOVE WRK-INP-CODAGE-DRTM    TO  WRK-6416E-DEPDC
           MOVE 'DCOM6416'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-6416E-ENTRADA
                                       WRK-6416S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-6416S-COD-RETORNO
               WHEN '0000'
                    MOVE WRK-6416S-NOME-DEPDC   TO WRK-INP-DESCAGEN-DRTM
                                                   WRK-OUT-DESCAGEN-DRTM

               WHEN '0003'
                    MOVE  ALL '*'               TO WRK-INP-DESCAGEN-DRTM
                                                   WRK-OUT-DESCAGEN-DRTM

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  WRK-6416S-COD-RETORNO
                                        TO WRK-OUT-MENSA-DRTM(1:4)
                    MOVE  '-'           TO WRK-OUT-MENSA-DRTM(6:1)
                    MOVE  WRK-6416S-MSG-RETORNO
                                        TO WRK-OUT-MENSA-DRTM(8:72)
           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-CALCULAR-DIGITO            SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0431'   USING WRK-CONTA-0431
                                   WRK-DIGITO-0431
                                   WRK-TAMANHO-0431.


           IF  WRK-DIGITO-0431  EQUAL  '.'
               MOVE 'APL'          TO ERR-TIPO-ACESSO
               MOVE 'ERRO NO CALCULO DO DIGITO - BRAD0431 - CONTA = '
                                   TO ERR-TEXTO(1:47)
               MOVE WRK-CONTA-0431      TO WRK-CONTA-0431-R
               MOVE WRK-CONTA-0431-R    TO ERR-TEXTO(48:13)
                   MOVE ' 1130-CALCULAR-DIGITO -ERRO'
                                       TO WRK-OUT-MENSA-DRTM
           ELSE
               IF  WRK-DIGITO-0431       EQUAL  'P'
                   MOVE ZEROS       TO WRK-DIGITO-0431-R
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *                    BUSCA NOME DO CLIENTE
      *----------------------------------------------------------------*
       1140-ACESSAR-DCOM6408           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE    ROTENT-6408-ENVIO.

           MOVE   ZEROS                TO  ROTENT-6408-COD-ENV
                                           ROTENT-6408-REST-ENV

           MOVE   SPACES               TO  ROTENT-6408-MSG-ENV
           MOVE   'N'                  TO  ROTENT-6408-FLAG-ENV
                                           ROTENT-6408-START-ENV

           MOVE  WRK-COD-USER          TO ROTENT-6408-CFUNC-ENV
           MOVE  LNK-IO-LTERM          TO ROTENT-6408-CTERM-ENV

           IF  WRK-INP-CODAGE-DRTM      IS  NUMERIC   AND
               WRK-INP-CODAGE-DRTM      GREATER  ZEROS
               MOVE  WRK-INP-CODAGE-DRTM TO ROTENT-6408-CJUNC-ENV
               MOVE  WRK-INP-CONTA-DRTM  TO ROTENT-6408-CCTAC-ENV
               MOVE  WRK-DIGITO-0431-R   TO ROTENT-6408-DCTAC-ENV
               MOVE ZEROS                TO ROTENT-6408-NUM-CNPJ-ENV
                                            ROTENT-6408-FIL-CNPJ-ENV
                                            ROTENT-6408-CTR-CNPJ-ENV
               MOVE 'BANCO:'             TO WRK-INP-BANCOX-DRTM
                                            WRK-OUT-BANCOX-DRTM
               MOVE 'AGENCIA:'           TO WRK-INP-AGENX-DRTM
                                            WRK-OUT-AGENX-DRTM
               MOVE 'CONTA...:'          TO WRK-INP-CONTAX-DRTM
                                            WRK-OUT-CONTAX-DRTM
           ELSE
               MOVE WRK-INP-NUMERO-DRTM
                                       TO ROTENT-6408-NUM-CNPJ-ENV
               MOVE WRK-INP-FILIAL-DRTM
                                       TO ROTENT-6408-FIL-CNPJ-ENV
               MOVE WRK-INP-CONTROLE-DRTM
                                       TO ROTENT-6408-CTR-CNPJ-ENV
               MOVE ZEROS              TO ROTENT-6408-CJUNC-ENV
                                          ROTENT-6408-CCTAC-ENV
                                          ROTENT-6408-DCTAC-ENV
               MOVE 'CNPJ/CPF:'        TO WRK-INP-CNPJX-DRTM
                                          WRK-OUT-CNPJX-DRTM
           END-IF.

           MOVE  'DCOM6408'            TO WRK-MODULO.

           CALL WRK-MODULO  USING ROTENT-6408-AREA
                                  ROTSAI-6408-RETORNO
                                  ERRO-AREA
                                  WRK-SQLCA.

           EVALUATE ROTSAI-6408-COD-RET

               WHEN '0000'

                   MOVE ROTSAI-6408-NM-RZSOC-RET(1)
                                       TO WRK-OUT-NMCLIE-DRTM
                                          WRK-INP-NMCLIE-DRTM

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   MOVE '0070'         TO ERR-LOCAL
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   INITIALIZE   COMU-AREA
                   MOVE ROTSAI-6408-MENS-RET TO COMU-MENSAG
                   PERFORM 1150-RETORNA-CHAMADOR-DCOM0413
           END-EVALUATE.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-RETORNA-CHAMADOR-DCOM0413  SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA     TO COMU-LL
           MOVE WRK-CHNG             TO WRK-FUNCAO
           MOVE 'DCOM0413'           TO WRK-TELA
           MOVE 'DCOM0418'           TO COMU-TRANSACAO
           MOVE WRK-INP-TIPOOPC-DRTM TO COMU-OPCAO
           MOVE WRK-INP-TMSTAMP-DRTM TO COMU-TIMESTAMP
           MOVE COMU-AREA            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RECEBE O CONTROLE DO DCOM0419                     *
      *----------------------------------------------------------------*
       1200-RECEBER-CONTROLE-419       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO  COMU-AREA-419.

           MOVE COMU-TMSTAMP-419       TO  WRK-INP-TMSTAMP-DRTM
                                           WRK-OUT-TMSTAMP-DRTM
           MOVE COMU-OPCAO-419         TO  WRK-INP-TIPOOPC-DRTM
                                           WRK-OUT-TIPOOPC-DRTM
           MOVE ZEROS                  TO  WRK-4111E-RESTART
                                           WRK-4112E-RESTART.
           MOVE 1                      TO  WRK-INP-PAGINA-DRTM-N
                                           WRK-OUT-PAGINA-DRTM-N

           IF COMU-CODAGE-419-X  IS NUMERIC AND
              COMU-CODAGE-419-N  IS GREATER ZEROS
              MOVE  1                  TO  WRK-INP-TIPO-PESQ-DRTM
                                           WRK-OUT-TIPO-PESQ-DRTM
              MOVE 237                 TO  WRK-INP-BANCO-DRTM
                                           WRK-OUT-BANCO-DRTM
              MOVE COMU-CODAGE-419-N   TO  WRK-INP-CODAGE-DRTM
                                           WRK-OUT-CODAGE-DRTM
                                           WRK-4111E-CAG-BCRIA
              MOVE '-'                 TO  WRK-INP-HIFEN-AG-DRTM
                                           WRK-OUT-HIFEN-AG-DRTM
              MOVE COMU-CONTA-419-N    TO  WRK-INP-CONTA-DRTM
                                           WRK-OUT-CONTA-DRTM
                                           WRK-4111E-CCTA-BCRIA

              PERFORM  1110-MONTAR-CABECALHO

              PERFORM 1500-ACESSAR-DCOM4111

           ELSE

              MOVE  2                  TO  WRK-INP-TIPO-PESQ-DRTM
                                           WRK-OUT-TIPO-PESQ-DRTM
              MOVE COMU-NUMERO-419-N   TO  WRK-INP-NUMERO-DRTM
                                           WRK-OUT-NUMERO-DRTM
                                           WRK-4112E-CCNPJ-CPF
              MOVE '/'                 TO  WRK-INP-BARRA-1-DRTM
                                           WRK-OUT-BARRA-1-DRTM
              MOVE COMU-FILIAL-419-N   TO  WRK-INP-FILIAL-DRTM
                                           WRK-OUT-FILIAL-DRTM
                                           WRK-4112E-CFLIAL-CNPJ
              MOVE '-'                 TO  WRK-INP-HIFEN-2-DRTM
                                           WRK-OUT-HIFEN-2-DRTM
              MOVE COMU-CONTROLE-419-N TO  WRK-INP-CONTROLE-DRTM
                                           WRK-OUT-CONTROLE-DRTM
                                           WRK-4112E-CCTRL-CNPJ-CPF

              PERFORM  1110-MONTAR-CABECALHO

              PERFORM 1600-ACESSAR-DCOM4112
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDRTM        *
      *----------------------------------------------------------------*
       1300-PROCESSAR-DCOMDRTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DRTM.
           PERFORM 1310-DEVOLVER-TELA-FIXA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-DRTM     EQUAL 'H' OR
                   (WRK-INP-PFK-DRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DRTM EQUAL 'PFK1')
                    PERFORM 1311-DEVOLVER-TELA-VARIA
                    MOVE WRK-INP-MENSA-DRTM TO WRK-OUT-MENSA-DRTM

               WHEN WRK-INP-PFK-DRTM     EQUAL '3' OR
                   (WRK-INP-PFK-DRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DRTM EQUAL 'PFK3')
                    PERFORM 1320-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DRTM     EQUAL 'X' OR
                   (WRK-INP-PFK-DRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DRTM EQUAL 'PFK5')
                    PERFORM 1330-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DRTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DRTM EQUAL 'PFK10')
                    PERFORM 1340-RETORNAR-MENU-DCOM

               WHEN WRK-INP-FASE-DRTM-N    EQUAL 1
                    PERFORM 1350-TRATAR-FASE-1

               WHEN WRK-INP-FASE-DRTM-N    EQUAL 2
                    PERFORM 1360-TRATAR-FASE-2

               WHEN OTHER
                    PERFORM 1311-DEVOLVER-TELA-VARIA
                    MOVE SG-MENSAGEM(38)  TO  WRK-OUT-MENSA-DRTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS FIXOS DA TELA SEM ALTERACAO      *
      *----------------------------------------------------------------*
       1310-DEVOLVER-TELA-FIXA         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-PAGINA-DRTM-X   TO WRK-OUT-PAGINA-DRTM-X.
           MOVE WRK-INP-FASE-DRTM-X     TO WRK-OUT-FASE-DRTM-X.
           MOVE WRK-INP-FIMAMOS-DRTM    TO WRK-OUT-FIMAMOS-DRTM.
           MOVE WRK-INP-NMCLIE-DRTM     TO WRK-OUT-NMCLIE-DRTM.
           MOVE WRK-INP-TIPO-PESQ-DRTM  TO WRK-OUT-TIPO-PESQ-DRTM.
           MOVE WRK-INP-TMSTAMP-DRTM    TO WRK-OUT-TMSTAMP-DRTM.
           MOVE WRK-INP-MENSA-DRTM      TO WRK-OUT-MENSA-DRTM.

           IF  WRK-INP-TIPO-PESQ-DRTM   EQUAL  1
               MOVE WRK-INP-BANCOX-DRTM     TO WRK-OUT-BANCOX-DRTM
               MOVE WRK-INP-AGENX-DRTM      TO WRK-OUT-AGENX-DRTM
               MOVE WRK-INP-CONTAX-DRTM     TO WRK-OUT-CONTAX-DRTM
               MOVE WRK-INP-BANCO-DRTM-X    TO WRK-OUT-BANCO-DRTM-X
               MOVE WRK-INP-AGENCIA-GR-DRTM TO WRK-OUT-AGENCIA-GR-DRTM
               MOVE WRK-INP-CONTA-DRTM      TO WRK-OUT-CONTA-DRTM
           ELSE
               MOVE WRK-INP-CNPJX-DRTM      TO WRK-OUT-CNPJX-DRTM
               MOVE WRK-INP-CNPJCPF-DRTM    TO WRK-OUT-CNPJCPF-DRTM
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS VARIAVEIS DA TELA S/ ALTERACAO   *
      *----------------------------------------------------------------*
       1311-DEVOLVER-TELA-VARIA        SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 05
              IF  WRK-INP-CONTR-DRTM-X(WRK-IND)   IS  NUMERIC  AND
                  WRK-INP-CONTR-DRTM-X(WRK-IND)   GREATER  ZEROS
                  MOVE WRK-INP-SELEC-DRTM (WRK-IND)
                                      TO WRK-OUT-SELEC-DRTM (WRK-IND)
                  MOVE WRK-INP-CONTR-DRTM-X(WRK-IND)
                                      TO WRK-OUT-CONTR-DRTM-X(WRK-IND)
                  MOVE WRK-INP-NOME-DRTM(WRK-IND)
                                      TO WRK-OUT-NOME-DRTM(WRK-IND)
                  MOVE WRK-INP-DTINIC-DRTM(WRK-IND)
                                      TO WRK-OUT-DTINIC-DRTM(WRK-IND)
                  MOVE WRK-INP-DTVENCT-DRTM(WRK-IND)
                                      TO WRK-OUT-DTVENCT-DRTM(WRK-IND)
                  MOVE WRK-INP-LIMITE-DRTM(WRK-IND)
                                      TO WRK-OUT-LIMITE-DRTM-X(WRK-IND)
                  MOVE WRK-INP-SITUAC-DRTM(WRK-IND)
                                      TO WRK-OUT-SITUAC-DRTM(WRK-IND)
              ELSE
                  MOVE WRK-241       TO WRK-OUT-SELEC-ATTR-DRTM(WRK-IND)
              END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1320-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE 'DCOM0413'           TO WRK-TELA.
           MOVE 'DCOM0418'           TO COMU-TRANSACAO-413.
           MOVE SPACES               TO COMU-DADOS-413.
           MOVE WRK-INP-TMSTAMP-DRTM TO COMU-TMSTAMP-413.
           MOVE WRK-INP-TIPOOPC-DRTM TO COMU-OPCAO-413.

           MOVE COMU-AREA-413        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1330-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG            TO WRK-FUNCAO.
           MOVE 'DCOM0418'          TO WRK-TELA
                                       COMU-TRANSACAO-413.
           MOVE '5'                 TO COMU-PFK-413.
           MOVE COMU-AREA-413       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1340-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0418'         TO WRK-TELA
                                      COMU-TRANSACAO-413.
           MOVE 'A'                TO COMU-PFK-413.
           MOVE COMU-AREA-413      TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1350-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE

               WHEN WRK-INP-PFK-DRTM     EQUAL '7' OR
                   (WRK-INP-PFK-DRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DRTM EQUAL 'PFK7')
                    PERFORM 1351-TRATAR-VOLTA-PAG

               WHEN WRK-INP-PFK-DRTM     EQUAL '8' OR
                   (WRK-INP-PFK-DRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DRTM EQUAL 'PFK8')
                    PERFORM 1352-TRATAR-AVANCA-PAG

               WHEN WRK-INP-PFK-DRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DRTM EQUAL  SPACES OR LOW-VALUES
                    PERFORM 1311-DEVOLVER-TELA-VARIA
                    PERFORM 1353-TRATAR-ENTER

               WHEN OTHER
                    PERFORM 1311-DEVOLVER-TELA-VARIA
                    MOVE SG-MENSAGEM (0038)
                                         TO WRK-OUT-MENSA-DRTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DO COMANDO PFK7 - VOLTA PAG.  *
      *----------------------------------------------------------------*
       1351-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-PAGINA-DRTM-N  EQUAL  1
              PERFORM 1311-DEVOLVER-TELA-VARIA
              MOVE SG-MENSAGEM (0031)
                                       TO WRK-OUT-MENSA-DRTM
              GO TO 1351-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DRTM      NOT EQUAL WRK-MENSA-PF7

               MOVE ZEROS            TO WRK-QTD-D
                                        WRK-QTD-C

               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL   WRK-IND GREATER 5
                   IF  WRK-INP-SELEC-DRTM(WRK-IND) EQUAL 'D'
                       ADD  1          TO WRK-QTD-D
                   ELSE
                       IF  WRK-INP-SELEC-DRTM(WRK-IND) EQUAL 'C'
                           ADD  1          TO WRK-QTD-C
                       END-IF
                   END-IF

               END-PERFORM

               IF  WRK-QTD-D      GREATER ZEROS OR
                   WRK-QTD-C      GREATER ZEROS
                   PERFORM 1311-DEVOLVER-TELA-VARIA
                   MOVE WRK-MENSA-PF7           TO  WRK-OUT-MENSA-DRTM
                   GO  TO 1351-99-FIM
               END-IF

           END-IF.

           MOVE  SPACES                TO WRK-OUT-TABELA-DRTM

           IF  WRK-INP-TIPO-PESQ-DRTM   EQUAL   1
               COMPUTE WRK-4111E-RESTART =
                                       (WRK-INP-PAGINA-DRTM-N - 2) * 5
               PERFORM 1500-ACESSAR-DCOM4111
           ELSE
               COMPUTE WRK-4112E-RESTART =
                                       (WRK-INP-PAGINA-DRTM-N - 2) * 5
               PERFORM 1600-ACESSAR-DCOM4112
           END-IF.

           COMPUTE WRK-OUT-PAGINA-DRTM-N = WRK-INP-PAGINA-DRTM-N - 1.

      *----------------------------------------------------------------*
       1351-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DO COMANDO PFK8 - AVANCA PAG. *
      *----------------------------------------------------------------*
       1352-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-FIMAMOS-DRTM  EQUAL  '*'
              PERFORM 1311-DEVOLVER-TELA-VARIA
              MOVE SG-MENSAGEM (0030)
                                       TO WRK-OUT-MENSA-DRTM
              GO TO 1352-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DRTM      NOT EQUAL WRK-MENSA-PF8

               MOVE ZEROS            TO WRK-QTD-D
                                        WRK-QTD-C

               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL   WRK-IND GREATER 5
                   IF  WRK-INP-SELEC-DRTM(WRK-IND) EQUAL 'D'
                       ADD  1          TO WRK-QTD-D
                   ELSE
                       IF  WRK-INP-SELEC-DRTM(WRK-IND) EQUAL 'C'
                           ADD  1          TO WRK-QTD-C
                       END-IF
                   END-IF

               END-PERFORM

               IF  WRK-QTD-D      GREATER ZEROS OR
                   WRK-QTD-C      GREATER ZEROS
                   PERFORM 1311-DEVOLVER-TELA-VARIA
                   MOVE WRK-MENSA-PF8           TO  WRK-OUT-MENSA-DRTM
                   GO  TO 1352-99-FIM
               END-IF

           END-IF.
           MOVE  SPACES                TO WRK-OUT-TABELA-DRTM

           IF  WRK-INP-TIPO-PESQ-DRTM   EQUAL   1
               COMPUTE WRK-4111E-RESTART =
                                        WRK-INP-PAGINA-DRTM-N * 5
               PERFORM 1500-ACESSAR-DCOM4111
           ELSE
               COMPUTE WRK-4112E-RESTART =
                                        WRK-INP-PAGINA-DRTM-N * 5
               PERFORM 1600-ACESSAR-DCOM4112
           END-IF.

           COMPUTE WRK-OUT-PAGINA-DRTM-N = WRK-INP-PAGINA-DRTM-N + 1.

      *----------------------------------------------------------------*
       1352-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DO COMANDO ENTER              *
      *----------------------------------------------------------------*
       1353-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO WRK-INCONSIS

           PERFORM 1354-CONSISTIR-FISICA

           IF WRK-INCONSIS        EQUAL  '*'
              GO TO 1353-99-FIM
           END-IF.

WIP001     PERFORM
WIP001       VARYING IND-1             FROM 1 BY 1
WIP001         UNTIL IND-1             GREATER 05
WIP001            IF  WRK-INP-SELEC-DRTM(IND-1)
WIP001                                 NOT EQUAL SPACES AND LOW-VALUES
WIP001                PERFORM 1355-VALIDAR-OPTIN
WIP001            END-IF
WIP001     END-PERFORM.
WIP001
WIP001     IF  WRK-INCONSIS            EQUAL '*'
WIP001         GO TO 1353-99-FIM
WIP001     END-IF.
WIP001
           PERFORM  VARYING  WRK-IND  FROM  1 BY 1
                      UNTIL  WRK-IND  GREATER 05
              IF  WRK-INP-SELEC-DRTM(WRK-IND)  EQUAL 'D'
                  PERFORM  1362-CHAMAR-TRAN-DCOM0419
                  GO TO 1353-99-FIM
              END-IF
           END-PERFORM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND > 5
                   MOVE WRK-225    TO WRK-OUT-SELEC-ATTR-DRTM(WRK-IND)
           END-PERFORM.

           MOVE 2                      TO WRK-OUT-FASE-DRTM-N.
           MOVE SG-MENSAGEM (0114)     TO WRK-OUT-MENSA-DRTM.

      *----------------------------------------------------------------*
       1353-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DO COMANDO ENTER              *
      *----------------------------------------------------------------*
       1354-CONSISTIR-FISICA           SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 05
             IF  WRK-INP-SELEC-DRTM(WRK-IND) NOT EQUAL 'C' AND 'D'  AND
                                                 SPACES  AND LOW-VALUES
                 MOVE '*'        TO WRK-INCONSIS
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DRTM(WRK-IND)
                 MOVE SG-MENSAGEM (0117)
                                 TO  WRK-OUT-MENSA-DRTM
                 GO TO 1354-99-FIM
             END-IF
           END-PERFORM.

           MOVE  ZEROS               TO  WRK-IND WRK-QTD-SPACES

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 05
             IF  WRK-INP-SELEC-DRTM(WRK-IND)   EQUAL SPACES
                 ADD  1          TO WRK-QTD-SPACES
             END-IF
           END-PERFORM.

           IF    WRK-QTD-SPACES                EQUAL 5
                 MOVE '*'            TO WRK-INCONSIS
                 MOVE SG-MENSAGEM (0053)
                                     TO WRK-OUT-MENSA-DRTM
                 GO TO 1354-99-FIM
           END-IF.

           MOVE  ZEROS               TO  WRK-QTD-C  WRK-QTD-D.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 05
             IF  WRK-INP-SELEC-DRTM(WRK-IND) EQUAL 'C'
                 ADD 1               TO  WRK-QTD-C
             ELSE
                 IF  WRK-INP-SELEC-DRTM(WRK-IND) EQUAL 'D'
                     ADD 1           TO  WRK-QTD-D
                 END-IF
             END-IF
           END-PERFORM.

           IF (WRK-QTD-C           GREATER  ZEROS AND
               WRK-QTD-D           GREATER  ZEROS)
               MOVE '*'                 TO WRK-INCONSIS
               MOVE SG-MENSAGEM(0118)   TO WRK-OUT-MENSA-DRTM
               PERFORM VARYING WRK-IND FROM 1 BY 1
                                               UNTIL WRK-IND GREATER 05
                    IF WRK-INP-SELEC-DRTM(WRK-IND) NOT EQUAL 'C' OR 'D'
                       MOVE WRK-49353
                                    TO WRK-OUT-SELEC-ATTR-DRTM(WRK-IND)
                    END-IF
               END-PERFORM
           END-IF.

           IF WRK-INCONSIS        EQUAL  '*'
              GO TO 1354-99-FIM
           END-IF.

           IF  WRK-QTD-D           GREATER    1
               MOVE '*'                 TO WRK-INCONSIS
               MOVE SG-MENSAGEM (0113)  TO WRK-OUT-MENSA-DRTM
               PERFORM VARYING WRK-IND FROM 1 BY 1
                                               UNTIL WRK-IND GREATER 05
                    IF WRK-INP-SELEC-DRTM(WRK-IND) NOT EQUAL 'D'
                       MOVE WRK-49353
                                    TO WRK-OUT-SELEC-ATTR-DRTM(WRK-IND)
                    END-IF
               END-PERFORM
           END-IF.

      *----------------------------------------------------------------*
       1354-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

WIP001*----------------------------------------------------------------*
WIP001 1355-VALIDAR-OPTIN              SECTION.
WIP001*----------------------------------------------------------------*
WIP001*
WIP001     INITIALIZE WRK-DCOM5124-ENTRADA
WIP001                WRK-DCOM5124-SAIDA
WIP001                ERRO-AREA
WIP001                WRK-SQLCA.
WIP001*
WIP001     MOVE WRK-INP-CONTR-DRTM(IND-1)
WIP001                                 TO WRK-5124E-CCONTR-LIM.
WIP001     MOVE ZEROS                  TO WRK-5124E-CVRSAO-CON.
WIP001     MOVE 'DCOM5124'             TO WRK-MODULO
WIP001*
WIP001     CALL WRK-MODULO             USING WRK-DCOM5124-ENTRADA
WIP001                                       WRK-DCOM5124-SAIDA
WIP001                                       ERRO-AREA
WIP001                                       WRK-SQLCA.
WIP001*
WIP001     EVALUATE WRK-5124S-COD-RETORNO
WIP001       WHEN 0000
WIP001         CONTINUE
WIP001*
WIP001       WHEN 0099
WIP001         MOVE 'DB2'              TO ERR-TIPO-ACESSO
WIP001         PERFORM 9999-PROCESSAR-ROTINA-ERRO
WIP001*
WIP001       WHEN OTHER
WIP001         MOVE WRK-5124S-MSG-RETORNO
WIP001                                 TO WRK-OUT-MENSA-DRTM
WIP001         MOVE WRK-49353          TO WRK-OUT-SELEC-ATTR-DRTM(IND-1)
WIP001         MOVE '*'                TO WRK-INCONSIS
WIP001     END-EVALUATE.
WIP001*
WIP001*----------------------------------------------------------------*
WIP001 1355-99-FIM.                    EXIT.
WIP001*----------------------------------------------------------------*
WIP001
      *----------------------------------------------------------------*
       1360-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE

               WHEN WRK-INP-PFK-DRTM     EQUAL '6' OR
                   (WRK-INP-PFK-DRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DRTM EQUAL 'PFK6')
                    PERFORM 1361-PF6-CONFIRMA

               WHEN  WRK-INP-PFK-DRTM     EQUAL '.' AND
                     WRK-INP-COMANDO-DRTM EQUAL  SPACES OR LOW-VALUES
                     PERFORM VARYING WRK-IND FROM 1 BY 1
                                                      UNTIL WRK-IND > 5
                           MOVE WRK-49345
                                     TO WRK-OUT-SELEC-ATTR-DRTM(WRK-IND)
                     END-PERFORM
                     PERFORM 1311-DEVOLVER-TELA-VARIA
               MOVE 'SELECIONE UM REGISTRO COM "D" OU VARIOS COM "C" E T
      -         'ECLE <ENTER>'     TO  WRK-OUT-MENSA-DRTM
                     MOVE  1         TO WRK-OUT-FASE-DRTM-N

               WHEN  OTHER
                     PERFORM 1311-DEVOLVER-TELA-VARIA
                     PERFORM VARYING WRK-IND FROM 1 BY 1
                                                      UNTIL WRK-IND > 5
                           MOVE WRK-225
                                     TO WRK-OUT-SELEC-ATTR-DRTM(WRK-IND)
                     END-PERFORM
                     MOVE SG-MENSAGEM(38)
                                     TO  WRK-OUT-MENSA-DRTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1361-PF6-CONFIRMA               SECTION.
      *----------------------------------------------------------------*

           PERFORM  VARYING  WRK-IND  FROM  1 BY 1
                      UNTIL  WRK-IND  GREATER 05
              IF  WRK-INP-SELEC-DRTM(WRK-IND)  EQUAL 'C'
                  PERFORM 1363-CHAMAR-MOD-DCOM5143
              END-IF
           END-PERFORM.

           IF  WRK-CANCELOU-OK      EQUAL   'S'
               IF  WRK-INP-TIPO-PESQ-DRTM  EQUAL  1
                   MOVE ZEROS      TO  WRK-4111E-RESTART
                   PERFORM 1500-ACESSAR-DCOM4111
               ELSE
                   MOVE ZEROS      TO  WRK-4112E-RESTART
                   PERFORM 1600-ACESSAR-DCOM4112
               END-IF
               MOVE 1              TO  WRK-INP-PAGINA-DRTM-N
                                       WRK-OUT-PAGINA-DRTM-N
                                       WRK-INP-FASE-DRTM-N
                                       WRK-OUT-FASE-DRTM-N
               MOVE SPACES         TO  WRK-INP-FIMAMOS-DRTM
                                       WRK-OUT-FIMAMOS-DRTM
               MOVE SG-MENSAGEM (0119)
                                   TO  WRK-OUT-MENSA-DRTM

               PERFORM VARYING WRK-IND FROM 1 BY 1
                               UNTIL WRK-IND > 5
                       MOVE WRK-49345
                                   TO WRK-OUT-SELEC-ATTR-DRTM(WRK-IND)
               END-PERFORM

           END-IF.

      *----------------------------------------------------------------*
       1361-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA TRAN. DCOM0419              *
      *----------------------------------------------------------------*
       1362-CHAMAR-TRAN-DCOM0419       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG                    TO WRK-FUNCAO.
           MOVE 'DCOM0419'                  TO WRK-TELA.
           MOVE LENGTH COMU-AREA-419        TO COMU-LL-419
           MOVE 'DCOM0418'                  TO COMU-TRANSACAO-419.
           MOVE WRK-INP-TIPOOPC-DRTM        TO COMU-OPCAO-419.
           MOVE WRK-INP-SENHAS-DRTM         TO COMU-SENHAS-419.
           MOVE WRK-INP-TMSTAMP-DRTM        TO COMU-TMSTAMP-419.
           MOVE WRK-INP-CONTR-DRTM(WRK-IND) TO COMU-CONTRATO-419.
           MOVE WRK-INP-PAGINA-DRTM-N       TO COMU-PAGINA-419.

           IF WRK-INP-AGENCIA-GR-DRTM  NOT EQUAL  SPACES
              MOVE WRK-INP-CODAGE-DRTM      TO COMU-CODAGE-419-N
              MOVE WRK-INP-CONTA-DRTM       TO COMU-CONTA-419-N
              MOVE SPACES                   TO COMU-NUMERO-419-X
                                               COMU-FILIAL-419-X
                                               COMU-CONTROLE-419-X
           ELSE
              MOVE WRK-INP-NUMERO-DRTM      TO COMU-NUMERO-419-N
              MOVE WRK-INP-FILIAL-DRTM      TO COMU-FILIAL-419-N
              MOVE WRK-INP-CONTROLE-DRTM    TO COMU-CONTROLE-419-N
              MOVE SPACES                   TO COMU-CODAGE-419-X
                                               COMU-CONTA-419-X
           END-IF.

           MOVE COMU-AREA-419               TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1362-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA MODULO DCOM5143             *
      *----------------------------------------------------------------*
       1363-CHAMAR-MOD-DCOM5143        SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-5143E-COD-RETORNO
           MOVE SPACES                 TO WRK-5143E-MSG-RETORNO
           MOVE ZEROS                  TO WRK-5143E-RESTART
           MOVE SPACES                 TO WRK-5143E-FLAG
           MOVE WRK-COD-USER           TO WRK-5143E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-5143E-CTERM
           MOVE WRK-INP-CONTR-DRTM(WRK-IND)
                                       TO WRK-5143E-CCONTR-LIM-DESC
           MOVE 'DCOM5143'             TO WRK-MODULO

           CALL WRK-MODULO    USING    WRK-5143E-ENTRADA
                                       WRK-5143S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-5143S-COD-RETORNO
               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN '0000'
                    MOVE  'S'      TO  WRK-CANCELOU-OK


               WHEN OTHER
                    PERFORM 1311-DEVOLVER-TELA-VARIA
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                                                      UNTIL WRK-IND > 5
                       MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DRTM(WRK-IND)
                    END-PERFORM
                    MOVE  WRK-5143S-MSG-RETORNO
                                             TO WRK-OUT-MENSA-DRTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1363-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM4111                           *
      *----------------------------------------------------------------*
       1500-ACESSAR-DCOM4111           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4111E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4111E-CTERM
           MOVE WRK-INP-CODAGE-DRTM    TO  WRK-4111E-CAG-BCRIA
           MOVE WRK-INP-CONTA-DRTM     TO  WRK-4111E-CCTA-BCRIA
           MOVE 'M'                    TO  WRK-4111E-FLAG-FILTRO

           MOVE 'DCOM4111'             TO  WRK-MODULO.

           CALL WRK-MODULO    USING    WRK-4111E-ENTRADA
                                       WRK-4111S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4111S-COD-RETORNO
               WHEN '0000'
                    IF (WRK-4111S-CCONTR-LIM-DESC(6)  NOT NUMERIC) OR
                       (WRK-4111S-CCONTR-LIM-DESC(6)  EQUAL ZEROS)
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DRTM
                       MOVE SG-MENSAGEM (0017)
                                    TO  WRK-OUT-MENSA-DRTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DRTM
                       MOVE SG-MENSAGEM (0001)
                                    TO  WRK-OUT-MENSA-DRTM
                    END-IF
                    PERFORM 1510-MONTAR-TELA-AGENCTA VARYING  WRK-IND
                              FROM  1 BY 1 UNTIL  WRK-IND  GREATER 05
               WHEN '0003'
                   INITIALIZE         COMU-AREA
                   IF  WRK-CANCELOU-OK      EQUAL   'S'
                       MOVE  SG-MENSAGEM(0203)       TO COMU-MENSAG
                       PERFORM 1150-RETORNA-CHAMADOR-DCOM0413
                   ELSE
                       MOVE  SG-MENSAGEM(0198)       TO COMU-MENSAG
                       PERFORM 1150-RETORNA-CHAMADOR-DCOM0413
                   END-IF

               WHEN '0088'
               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   INITIALIZE COMU-AREA
                   MOVE  WRK-4111S-MSG-RETORNO   TO COMU-MENSAG
                   PERFORM 1150-RETORNA-CHAMADOR-DCOM0413

           END-EVALUATE.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA A PARTIR DO DCOM4111    *
      *----------------------------------------------------------------*
       1510-MONTAR-TELA-AGENCTA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES             TO WRK-OUT-SELEC-DRTM(WRK-IND)

           IF  WRK-4111S-CCONTR-LIM-DESC(WRK-IND) IS NUMERIC AND
               WRK-4111S-CCONTR-LIM-DESC(WRK-IND) GREATER ZERO
               MOVE SPACES        TO WRK-OUT-SELEC-DRTM(WRK-IND)
               MOVE WRK-4111S-CCONTR-LIM-DESC(WRK-IND)
                                  TO WRK-OUT-CONTR-DRTM(WRK-IND)
               MOVE WRK-4111S-NM-CLIE(WRK-IND)
                                  TO WRK-OUT-NOME-DRTM(WRK-IND)
               MOVE WRK-4111S-DINIC-VGCIA-CONTR(WRK-IND)
                                  TO WRK-OUT-DTINIC-DRTM(WRK-IND)
               INSPECT WRK-OUT-DTINIC-DRTM(WRK-IND) REPLACING ALL
                                                    '.' BY '/'
               MOVE WRK-4111S-DVCTO-CONTR-LIM(WRK-IND)
                                  TO WRK-OUT-DTVENCT-DRTM(WRK-IND)
               INSPECT WRK-OUT-DTVENCT-DRTM(WRK-IND) REPLACING ALL
                                                    '.' BY '/'
               MOVE WRK-4111S-VCONTR-LIM(WRK-IND) TO WRK-EDT-VAL
               MOVE WRK-EDT-VAL   TO WRK-OUT-LIMITE-DRTM-X(WRK-IND)
               MOVE WRK-4111S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITUAC-DRTM(WRK-IND)
           ELSE
               MOVE WRK-241       TO WRK-OUT-SELEC-ATTR-DRTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM4112                           *
      *----------------------------------------------------------------*
       1600-ACESSAR-DCOM4112           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4112E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4112E-CTERM
           MOVE WRK-INP-NUMERO-DRTM    TO WRK-4112E-CCNPJ-CPF
           MOVE WRK-INP-FILIAL-DRTM    TO WRK-4112E-CFLIAL-CNPJ
           MOVE WRK-INP-CONTROLE-DRTM  TO WRK-4112E-CCTRL-CNPJ-CPF
           MOVE 'M'                    TO  WRK-4112E-FLAG-FILTRO
           MOVE WRK-COD-DEPTO-N        TO  WRK-AGENCIA-NUM.
           MOVE WRK-AGENCIA-AUX        TO  WRK-4112E-MSG-RETORNO(1:5).

           MOVE 'DCOM4112'             TO  WRK-MODULO.

           CALL WRK-MODULO    USING    WRK-4112E-ENTRADA
                                       WRK-4112S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4112S-COD-RETORNO
               WHEN '0000'
                    IF (WRK-4112S-CCONTR-LIM-DESC(6)  NOT NUMERIC) OR
                       (WRK-4112S-CCONTR-LIM-DESC(6)  EQUAL ZEROS)
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DRTM
                       MOVE SG-MENSAGEM (0017)
                                    TO  WRK-OUT-MENSA-DRTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DRTM
                       MOVE SG-MENSAGEM (0001)
                                    TO  WRK-OUT-MENSA-DRTM
                    END-IF
                    PERFORM 1610-MONTAR-TELA-CNPJCPF VARYING  WRK-IND
                                FROM  1 BY 1 UNTIL  WRK-IND  GREATER 05
               WHEN '0003'
                    INITIALIZE          COMU-AREA
                    IF  WRK-CANCELOU-OK      EQUAL   'S'
                        MOVE  SG-MENSAGEM(0203)       TO COMU-MENSAG
                        PERFORM 1150-RETORNA-CHAMADOR-DCOM0413
                    ELSE
                        MOVE  SG-MENSAGEM(0198)       TO COMU-MENSAG
                        PERFORM 1150-RETORNA-CHAMADOR-DCOM0413
                    END-IF

               WHEN '0088'
               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN '0915'
                    INITIALIZE        COMU-AREA
                    IF  WRK-INP-TIPO-PESQ-DRTM  EQUAL  1
                        MOVE WRK-INP-CODAGE-DRTM    TO COMU-AGENCIA
                        MOVE WRK-INP-CONTA-DRTM     TO COMU-CONTA
                    ELSE
                        MOVE WRK-INP-CNPJCPF-DRTM(1:3)
                                                    TO COMU-CPF-X(1:3)
                        MOVE WRK-INP-CNPJCPF-DRTM(5:3)
                                                    TO COMU-CPF-X(4:3)
                        MOVE WRK-INP-CNPJCPF-DRTM(9:3)
                                                    TO COMU-CPF-X(7:3)
                        MOVE WRK-INP-FILIAL-DRTM    TO COMU-FILIAL
                        MOVE WRK-INP-CONTROLE-DRTM  TO COMU-DIGITO
                    END-IF
                    MOVE  WRK-4112S-MSG-RETORNO   TO COMU-MENSAG
                    PERFORM 1150-RETORNA-CHAMADOR-DCOM0413

               WHEN OTHER
                    INITIALIZE        COMU-AREA
                    MOVE  WRK-4112S-MSG-RETORNO   TO COMU-MENSAG
                    PERFORM 1150-RETORNA-CHAMADOR-DCOM0413

           END-EVALUATE.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA A PARTIR DO DCOM4111    *
      *----------------------------------------------------------------*
       1610-MONTAR-TELA-CNPJCPF        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES             TO WRK-OUT-SELEC-DRTM(WRK-IND)

           IF  WRK-4112S-CCONTR-LIM-DESC(WRK-IND) IS NUMERIC AND
               WRK-4112S-CCONTR-LIM-DESC(WRK-IND) GREATER ZERO
               MOVE SPACES        TO WRK-OUT-SELEC-DRTM(WRK-IND)
               MOVE WRK-4112S-CCONTR-LIM-DESC(WRK-IND)
                                  TO WRK-OUT-CONTR-DRTM(WRK-IND)
               MOVE WRK-4112S-NM-CLIE(WRK-IND)
                                  TO WRK-OUT-NOME-DRTM(WRK-IND)
               MOVE WRK-4112S-DINIC-VGCIA-CONTR(WRK-IND)
                                  TO WRK-OUT-DTINIC-DRTM(WRK-IND)
               MOVE WRK-4112S-DVCTO-CONTR-LIM(WRK-IND)
                                  TO WRK-OUT-DTVENCT-DRTM(WRK-IND)
               MOVE WRK-4112S-VCONTR-LIM(WRK-IND) TO WRK-EDT-VAL
               MOVE WRK-EDT-VAL   TO WRK-OUT-LIMITE-DRTM-X(WRK-IND)
               MOVE WRK-4112S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITUAC-DRTM(WRK-IND)
           ELSE
               MOVE WRK-225       TO WRK-OUT-SELEC-ATTR-DRTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1700-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-DRTM
                                             WRK-660-DCOMDRTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DRTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FINALIZA O PROCESSAMENTO DO PROGRAMA              *
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FORMATA E EMITE A MENSAGEM DE ERRO DO PROGRAMA    *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES AND LOW-VALUES
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0418'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0418'         TO ERR-PGM
           END-IF.


           MOVE WRK-COD-USER           TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.


           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
