      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM0502.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0502                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: DANILO                                       *
      *     DATA........: 01/09/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: ESTORNO DE OPERACAO - MENU                   *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMEBTM....: ESTORNO DE OPERACAO - MENU                   *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#DCOM11 - AREA PARA ACESSAR CNPJ/CPF                      *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSY - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *     I#DCOMX3 - AREA DE COMUNICAO DCOM6408.                     *
      *     I#DCOMXE - AREA DE COMUNICAO DCOM6437.                     *
      *     I#DCOMXF - AREA DE COMUNICAO DCOM6438.                     *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     BRAD7600 - FORNECE DATA E HORA CORRENTE DO SISTEMA         *
      *     DCOM6408 - VALIDAR CNPJ/CPF OU AGENCIA E CONTA.            *
      *     DCOM6437 - VALIDAR PERFIL DO USUARIO DO SENHAS.            *
      *     DCOM6438 - VERIFICAR DISPONIBILIDADE DO SISTEMA DCOM.      *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0501           CHAMA: DCOM0503           *
      *                                             DCOM0504           *
      *                                             DCOM0505           *
      *                                             DCOM0506           *
      *                                             DCOM0507           *
      *                                                                *
      *================================================================*

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
       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM0502 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  IND-INI                     PIC S9(09) COMP     VALUE ZEROS.
       77  WIND                        PIC S9(09) COMP     VALUE ZEROS.
       77  WCT-SELEC                   PIC S9(09) COMP     VALUE ZEROS.
       77  WRK-EDT-PAG                 PIC Z.ZZ9.
       77  WRK-EDT-VAL                 PIC ZZZ.ZZZ.ZZ9,99.
       77  WRK-TIPO-SELECAO            PIC  X(01)          VALUE SPACES.
       77  WRK-TIMESTAMP               PIC  X(26)          VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-241                     PIC S9(08) COMP     VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS. CURSOR    ---*
       01  WRK-49361                   PIC S9(08) COMP     VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                   PIC S9(08) COMP     VALUE +49369.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(08)          VALUE SPACES.
           05  WRK-AGENCIA-NUM         PIC  9(05)          VALUE ZEROS.
           05  WRK-AGENCIA-TELA        PIC  9(05)          VALUE ZEROS.
           05  WRK-INCONSIS            PIC  X(01)          VALUE SPACES.
           05  WRK-MSG-DCOM7995.
               10  FILLER              PIC  X(36)          VALUE
                   'ERRO NO ACESSO AO MODULO DCOM7995 - '.
               10  WRK-TEXTO           PIC  X(43)          VALUE SPACES.

           05  WRK-MSG-ERRO01          PIC  X(80)          VALUE
               'ENTRE COM UMA DAS COMBINACOES: OPERACAO OU AGENCIA OU AG
      -        'ENCIA/CONTA OU CNPJ/CPF'.


       01  WRK-CPFCGC.
FS2511*    05  WRK-NUMERO              PIC  9(009)         VALUE ZEROS.
FS2511     05  WRK-NUMERO              PIC  X(09)          VALUE SPACES.
FS2511*    05  WRK-FILIAL              PIC  9(005)         VALUE ZEROS.
FS2511     05  WRK-FILIAL              PIC  X(04)          VALUE SPACES.

       01  WRK-CONTROLE                PIC  X(002)         VALUE SPACES.

       01  WRK-AREA-BRAD0431.
           05  WRK-CONTA-0431          PIC  9(013) COMP-3  VALUE ZEROS.
           05  WRK-TAMANHO-0431        PIC  9(002)         VALUE ZEROS.
           05  WRK-DIGITO-0431         PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM0505 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK            PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-TPOPCAO        PIC  X(001)         VALUE SPACES.
               10  COMU-TIMESTAMP      PIC  X(026)         VALUE SPACES.
FS2511*        10  COMU-CNPJCPF        PIC  9(009)         VALUE ZEROS.
FS2511         10  COMU-CNPJCPF        PIC  X(09)          VALUE SPACES.
FS2511*        10  COMU-FILIAL         PIC  9(004)         VALUE ZEROS.
FS2511         10  COMU-FILIAL         PIC  X(04)          VALUE SPACES.
               10  COMU-CONTROLE       PIC  9(002)         VALUE ZEROS.
               10  COMU-BANCO          PIC  9(003)         VALUE ZEROS.
               10  COMU-AGENCIA        PIC  9(005)         VALUE ZEROS.
               10  COMU-CONTA          PIC  9(013)         VALUE ZEROS.
               10  COMU-OPERACAO       PIC  9(013)         VALUE ZEROS.
               10  FILLER REDEFINES    COMU-OPERACAO.
                   15 COMU-DANO        PIC  9(004).
                   15 COMU-NSEQ        PIC  9(009).
               10  COMU-CONTRATO-LIM   PIC  9(009)         VALUE ZEROS.
               10  COMU-PAGINA1        PIC  9(004)         VALUE ZEROS.
               10  COMU-OCCURS         PIC  9(008)         VALUE ZEROS.
           05  COMU-MENSAGEM           PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO PFK ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-PFK.
           05  COMU-LL-PFK             PIC S9(04) COMP     VALUE +124.
           05  COMU-ZZ-PFK             PIC S9(04) COMP     VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-PFK  PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(06)          VALUE SPACES.
               10  COMU-PFK-PFK        PIC  X(01)          VALUE SPACES.
           05  COMU-SENHAS-PFK         PIC  X(37)          VALUE SPACES.
           05  COMU-COMANDO-PFK        PIC  X(68)          VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)      VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(04)          VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(04)          VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(04)          VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(04)          VALUE SPACES.
           05  WRK-TELA                PIC  X(08)          VALUE SPACES.
           05  WRK-VERSAO              PIC  X(06)          VALUE
                                                           'VRS002'.
           05  WRK-COD-USER            PIC  X(07)          VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(07).
           05  WRK-COD-DEPTO.
               10  FILLER                  PIC  9(02)     VALUE ZEROS.
               10  WRK-COD-DEPTO-N         PIC  9(04)     VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(04)          VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(45)          VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(12)          VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE         PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(11)          VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(02)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOMEBTM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-EBTM.
           05  FILLER                  PIC  X(18)          VALUE SPACES.
           05  WRK-INP-PFK-EBTM        PIC  X(01)          VALUE SPACES.
           05  WRK-INP-SENHAS-EBTM     PIC  X(37)          VALUE SPACES.
           05  WRK-INP-COMANDO-EBTM    PIC  X(68)          VALUE SPACES.
           05  WRK-INP-DADOS-EBTM.
               10  WRK-INP-TPOPCAO-EBTM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-INP-TIMESTAMP-EBTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-INP-OPCAO-EBTM  PIC  X(01)          VALUE SPACES.
               10  WRK-INP-OPER-EBTM.
                   15  WRK-INP-OPER-EBTM-N
                                       PIC  9(04)          VALUE ZEROS.
               10  WRK-INP-NSEQ-EBTM.
                   15  WRK-INP-NSEQ-EBTM-N
                                       PIC  9(09)          VALUE ZEROS.
               10  WRK-INP-BANCO-EBTM  PIC  X(03)          VALUE SPACES.
               10  WRK-INP-AGENCIA-EBTM.
                   15  WRK-INP-AGENCIA-EBTM-N
                                       PIC  9(05)          VALUE ZEROS.
               10  WRK-INP-BANCO2-EBTM PIC  X(03)          VALUE SPACES.
               10  WRK-INP-AGENC2-EBTM.
                   15  WRK-INP-AGENC2-EBTM-N
                                       PIC  9(05)          VALUE ZEROS.
               10  WRK-INP-CONTA-EBTM.
                   15  WRK-INP-CONTA-EBTM-N
                                       PIC  9(13)          VALUE ZEROS.
               10  WRK-INP-CPFCNPJ-EBTM.
                   15  WRK-INP-NUMERO-EBTM.
                       20  WRK-INP-NUMERO-EBTM-N
                                       PIC  9(09)          VALUE ZEROS.
                   15  WRK-INP-FILIAL-EBTM.
                       20  WRK-INP-FILIAL-EBTM-N
                                       PIC  9(04)          VALUE ZEROS.
                   15  WRK-INP-CONTROLE-EBTM.
                       20  WRK-INP-CONTROLE-EBTM-N
                                       PIC  9(02)          VALUE ZEROS.
           05  WRK-INP-MENSA-EBTM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMEBTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-EBTM.
           05  WRK-OUT-LL-EBTM         PIC S9(04) COMP     VALUE +0295.
           05  WRK-OUT-ZZ-EBTM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-DADOS-EBTM.
               10  WRK-OUT-SENHAS-EBTM  PIC  X(37)         VALUE SPACES.
               10  WRK-OUT-COMANDO-EBTM PIC  X(68)         VALUE SPACES.
               10  WRK-OUT-TPOPCAO-EBTM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-TIMESTAMP-EBTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-OPCAO-ATTR-EBTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-OPCAO-EBTM  PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-OPER-ATTR-EBTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-OPER-EBTM.
                   15  WRK-OUT-OPER-EBTM-N
                                       PIC  9(04)          VALUE ZEROS.
               10  WRK-OUT-NSEQ-ATTR-EBTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-NSEQ-EBTM.
                   15  WRK-OUT-NSEQ-EBTM-N
                                       PIC  9(09)          VALUE ZEROS.
               10  WRK-OUT-BANCO-ATTR-EBTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-BANCO-EBTM  PIC  X(03)          VALUE SPACES.
               10  WRK-OUT-AGENCIA-ATTR-EBTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-AGENCIA-EBTM.
                   15  WRK-OUT-AGENCIA-EBTM-N
                                       PIC  9(05)          VALUE ZEROS.
               10  WRK-OUT-BANCO2-ATTR-EBTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-BANCO2-EBTM PIC  X(03)          VALUE SPACES.
               10  WRK-OUT-AGENC2-ATTR-EBTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-AGENC2-EBTM.
                   15  WRK-OUT-AGENC2-EBTM-N
                                       PIC  9(05)          VALUE ZEROS.
               10  WRK-OUT-CONTA-ATTR-EBTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-CONTA-EBTM.
                   15  WRK-OUT-CONTA-EBTM-N
                                       PIC  9(13)          VALUE ZEROS.
               10  WRK-OUT-NUMERO-ATTR-EBTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-NUMERO-EBTM.
                   15  WRK-OUT-NUMERO-EBTM-N
                                       PIC  9(09)          VALUE ZEROS.
               10  WRK-OUT-FILIAL-ATTR-EBTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-FILIAL-EBTM.
                   15  WRK-OUT-FILIAL-EBTM-N
                                       PIC  9(04)          VALUE ZEROS.
               10  WRK-OUT-CONTROLE-ATTR-EBTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-CONTROLE-EBTM.
                   15  WRK-OUT-CONTROLE-EBTM-N
                                       PIC  9(02)          VALUE ZEROS.
           05  WRK-OUT-MENSA-EBTM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOMEBTM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMEBTM.
           05  WRK-660-LL-AREA-EBTM      PIC  9(004) COMP    VALUE 0036.
           05  WRK-660-LL-MENSA-EBTM     PIC  9(004) COMP    VALUE 0295.
           05  WRK-660-SENHAS-EBTM       PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-EBTM      PIC  9(004) COMP    VALUE 0068.
           05  WRK-660-TPOPCAO-EBTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TIMESTAMP-EBTM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-OPCAO-EBTM        PIC  9(004) COMP    VALUE 2003.
           05  WRK-660-OPER-EBTM         PIC  9(004) COMP    VALUE 2006.
           05  WRK-660-NSEQ-EBTM         PIC  9(004) COMP    VALUE 2011.
           05  WRK-660-BANCO-EBTM        PIC  9(004) COMP    VALUE 2005.
           05  WRK-660-AGENCIA-EBTM      PIC  9(004) COMP    VALUE 2007.
           05  WRK-660-BANCO-EBTM        PIC  9(004) COMP    VALUE 2005.
           05  WRK-660-AGENC2-EBTM       PIC  9(004) COMP    VALUE 2007.
           05  WRK-660-CONTA-EBTM        PIC  9(004) COMP    VALUE 2015.
           05  WRK-660-CPFCNPJ-EBTM      PIC  9(004) COMP    VALUE 2011.
           05  WRK-660-FILIAL-EBTM       PIC  9(004) COMP    VALUE 2006.
           05  WRK-660-CONTROLE-EBTM     PIC  9(004) COMP    VALUE 2004.
           05  WRK-660-MENSA-EBTM        PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO DCOM5425 ***'.
      *----------------------------------------------------------------*

       01  ENTRADA-5425-ROTEADOR.
           05  RE5425-COD-RETORNO           PIC  X(004).
           05  RE5425-MSG-RETORNO           PIC  X(079).
           05  RE5425-RESTART               PIC  9(005).
           05  RE5425-FLAG                  PIC  X(001).
           05  RE5425-CFUNC-BDSCO           PIC  9(009).
           05  RE5425-CTERM                 PIC  X(008).
           05  RE5425-TIPO-CONSULTA         PIC  X(008).
           05  RE5425-OPERACAO.
            10 RE5425-DANO-OPER-DESC        PIC  9(004).
            10 RE5425-NSEQ-OPER-DESC        PIC  9(009).
           05  RE5425-CONTRATO REDEFINES RE5425-OPERACAO.
            10 RE5425-CCONTR-LIM-DESC       PIC  9(009).
            10 FILLER                       PIC  X(004).

        01  SAIDA-5425-ROTEADOR.
            05  RS5425-HEADER.
                10  RS5425-COD-RETORNO       PIC  X(004).
                10  RS5425-MSG-RETORNO       PIC  X(079).
                10  RS5425-RESTART           PIC  9(005).
                10  RS5425-FLAG              PIC  X(001).
            05  RS5425-CONSISTENCIA.
                10  RS5425-TAMANHO-ERRO      PIC  9(001).
                10  RS5425-TABELA-ERROS      OCCURS 002  TIMES.
                    15  RS5425-CAMPOS-ERRO   PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                        PIC  X(050)      VALUE
           '*** AREA DO DCOM5426 ***'.
      *----------------------------------------------------------------*
       01  ENTRADA-5426-ROTEADOR.
           05  RE5426-COD-RETORNO           PIC  X(004).
           05  RE5426-MSG-RETORNO           PIC  X(079).
           05  RE5426-RESTART               PIC  9(005).
           05  RE5426-FLAG                  PIC  X(001).
           05  RE5426-CFUNC-BDSCO           PIC  9(009).
           05  RE5426-CTERM                 PIC  X(008).
           05  RE5426-OPERACAO              PIC  9(013).
           05  FILLER      REDEFINES RE5426-OPERACAO.
            10 RE5426-DANO-OPER-DESC        PIC  9(004).
            10 RE5426-NSEQ-OPER-DESC        PIC  9(009).
           05  FILLER      REDEFINES RE5426-OPERACAO.
            10 RE5426-CCONTR-LIM-DESC       PIC  9(009).
            10 FILLER                       PIC  X(004).
           05  RE5426-TIPO-OPERACAO         PIC  X(007).
           05  RE5426-TIPO-CONSULTA         PIC  X(001).

       01  SAIDA-5426-ROTEADOR.
           05  RS5426-HEADER.
               10  RS5426-COD-RETORNO       PIC  X(004).
               10  RS5426-MSG-RETORNO       PIC  X(079).
               10  RS5426-RESTART           PIC  9(005).
               10  RS5426-FLAG              PIC  X(001).
           05  RS5426-CONSISTENCIA.
               10  RS5426-TAMANHO-ERRO      PIC  9(001).
               10  RS5426-TABELA-ERROS      OCCURS 004  TIMES.
                   15  RS5426-CAMPOS-ERRO   PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM6408 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMX3'.

      *----------------------------------------------------------------*
       01  FILLER                        PIC  X(050)      VALUE
           '*** AREA DO DCOM6437 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM6438 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXF'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM0502 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-IO-STATUS            PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-IO-MODNAME           PIC  X(08).

       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-ALT-STATUS           PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-ALT-MODNAME          PIC  X(08).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

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

           IF  RETURN-CODE             EQUAL 04 OR 12
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-EBTM.

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO
                      ERRO-AREA.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMEBTM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0502'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMEBTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMEBTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO COMU-AREA.
           MOVE COMU-TIMESTAMP         TO WRK-INP-TIMESTAMP-EBTM
                                          WRK-OUT-TIMESTAMP-EBTM.
           MOVE  '237'                 TO WRK-OUT-BANCO-EBTM
                                          WRK-OUT-BANCO2-EBTM

           IF COMU-MENSAGEM            EQUAL SPACES
              MOVE  SG-MENSAGEM(11)    TO WRK-OUT-MENSA-EBTM
           ELSE
              MOVE COMU-MENSAGEM       TO WRK-OUT-MENSA-EBTM
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMEBTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-EBTM.

           PERFORM 2210-1-EFETUAR-INSPECT.

           PERFORM 2210-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-EBTM     EQUAL 'H' OR
                   (WRK-INP-PFK-EBTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EBTM EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-EBTM    TO WRK-OUT-MENSA-EBTM
                    PERFORM 2210-DEVOLVER-TELA

               WHEN WRK-INP-PFK-EBTM     EQUAL '3' OR
                   (WRK-INP-PFK-EBTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EBTM EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-EBTM     EQUAL 'X' OR
                   (WRK-INP-PFK-EBTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EBTM EQUAL 'PFK05')
                    PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-EBTM     EQUAL 'V' OR
                   (WRK-INP-PFK-EBTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EBTM EQUAL 'PFK10')
                    PERFORM 2240-MENU-DCOM

               WHEN (WRK-INP-PFK-EBTM          EQUAL '.' AND
                     WRK-INP-COMANDO-EBTM      EQUAL SPACES)
                     PERFORM 2250-TRATAR-ENTER

               WHEN  OTHER
                     MOVE SG-MENSAGEM(38)
                                       TO  WRK-OUT-MENSA-EBTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-1-EFETUAR-INSPECT          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-AGENCIA-EBTM    NOT EQUAL   SPACES
               INSPECT WRK-INP-AGENCIA-EBTM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-OPER-EBTM       NOT EQUAL   SPACES
               INSPECT WRK-INP-OPER-EBTM   REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-NSEQ-EBTM       NOT EQUAL   SPACES
               INSPECT WRK-INP-NSEQ-EBTM   REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-AGENC2-EBTM     NOT EQUAL   SPACES
               INSPECT WRK-INP-AGENC2-EBTM  REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CONTA-EBTM      NOT EQUAL   SPACES
               INSPECT WRK-INP-CONTA-EBTM   REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-NUMERO-EBTM     NOT EQUAL   SPACES
               INSPECT WRK-INP-NUMERO-EBTM  REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-FILIAL-EBTM     NOT EQUAL   SPACES
               INSPECT WRK-INP-FILIAL-EBTM  REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CONTROLE-EBTM     NOT EQUAL   SPACES
               INSPECT WRK-INP-CONTROLE-EBTM
                                           REPLACING LEADING ' ' BY '0'
           END-IF.

      *----------------------------------------------------------------*
       2210-1-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-BANCO-EBTM     TO WRK-OUT-BANCO-EBTM
           MOVE WRK-INP-BANCO2-EBTM    TO WRK-OUT-BANCO2-EBTM
           MOVE WRK-INP-TPOPCAO-EBTM   TO WRK-OUT-TPOPCAO-EBTM.
           MOVE WRK-INP-TIMESTAMP-EBTM TO WRK-OUT-TIMESTAMP-EBTM.
           MOVE WRK-INP-OPCAO-EBTM     TO WRK-OUT-OPCAO-EBTM.
           MOVE WRK-INP-AGENCIA-EBTM   TO WRK-OUT-AGENCIA-EBTM.
           MOVE WRK-INP-OPER-EBTM      TO WRK-OUT-OPER-EBTM.
           MOVE WRK-INP-NSEQ-EBTM      TO WRK-OUT-NSEQ-EBTM.
           MOVE WRK-INP-AGENC2-EBTM    TO WRK-OUT-AGENC2-EBTM.
           MOVE WRK-INP-CONTA-EBTM     TO WRK-OUT-CONTA-EBTM.
           MOVE WRK-INP-NUMERO-EBTM    TO WRK-OUT-NUMERO-EBTM.
           MOVE WRK-INP-FILIAL-EBTM    TO WRK-OUT-FILIAL-EBTM.
           MOVE WRK-INP-CONTROLE-EBTM  TO WRK-OUT-CONTROLE-EBTM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF COMU-AREA        TO COMU-LL
           MOVE ZEROS                      TO COMU-ZZ
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0501'                 TO WRK-TELA
           MOVE 'DCOM0502'                 TO COMU-TRANSACAO
           MOVE WRK-INP-TIMESTAMP-EBTM     TO COMU-TIMESTAMP
           MOVE COMU-AREA                  TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2230-MENU-ROTINAS               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-PFK

           MOVE LENGTH OF COMU-AREA-PFK
                                       TO COMU-LL-PFK
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM0502'             TO WRK-TELA
                                          COMU-TRANSACAO-PFK
           MOVE '5'                    TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2240-MENU-DCOM                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-PFK

           MOVE LENGTH OF COMU-AREA-PFK
                                       TO COMU-LL-PFK
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM0502'             TO WRK-TELA
                                          COMU-TRANSACAO-PFK
           MOVE 'A'                    TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2250-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO WRK-INCONSIS

           IF  WRK-INP-OPCAO-EBTM      NOT EQUAL '1' AND '2' AND '3'
               MOVE SG-MENSAGEM(35)    TO WRK-OUT-MENSA-EBTM
               MOVE   WRK-49369        TO WRK-OUT-OPCAO-ATTR-EBTM
               GO                      TO 2250-99-FIM
           END-IF.

           IF  (WRK-INP-AGENCIA-EBTM       EQUAL SPACES  OR LOW-VALUES)
           AND (WRK-INP-OPER-EBTM          EQUAL SPACES  OR LOW-VALUES)
           AND (WRK-INP-NSEQ-EBTM          EQUAL SPACES  OR LOW-VALUES)
           AND (WRK-INP-AGENC2-EBTM        EQUAL SPACES  OR LOW-VALUES)
           AND (WRK-INP-CONTA-EBTM         EQUAL SPACES  OR LOW-VALUES)
           AND (WRK-INP-CPFCNPJ-EBTM       EQUAL SPACES  OR LOW-VALUES)
               MOVE WRK-MSG-ERRO01     TO  WRK-OUT-MENSA-EBTM
               MOVE '*'                TO WRK-INCONSIS
               MOVE WRK-49369          TO  WRK-OUT-OPER-ATTR-EBTM
                                           WRK-OUT-NSEQ-ATTR-EBTM
                                           WRK-OUT-AGENCIA-ATTR-EBTM
                                           WRK-OUT-AGENC2-ATTR-EBTM
                                           WRK-OUT-CONTA-ATTR-EBTM
                                           WRK-OUT-NUMERO-ATTR-EBTM
                                           WRK-OUT-FILIAL-ATTR-EBTM
                                           WRK-OUT-CONTROLE-ATTR-EBTM
               GO                      TO 2250-99-FIM
           END-IF.

           IF  (WRK-INP-OPER-EBTM       NOT EQUAL  SPACES   OR
                WRK-INP-NSEQ-EBTM       NOT EQUAL  SPACES)
               IF  WRK-INP-AGENCIA-EBTM  NOT EQUAL  SPACES  OR
                   WRK-INP-AGENC2-EBTM   NOT EQUAL  SPACES  OR
                   WRK-INP-CONTA-EBTM    NOT EQUAL  SPACES  OR
                   WRK-INP-CPFCNPJ-EBTM  NOT EQUAL  SPACES
                   MOVE WRK-MSG-ERRO01   TO  WRK-OUT-MENSA-EBTM
                   MOVE '*'              TO  WRK-INCONSIS
                   MOVE WRK-49369        TO  WRK-OUT-OPER-ATTR-EBTM
                                             WRK-OUT-NSEQ-ATTR-EBTM
                                             WRK-OUT-AGENCIA-ATTR-EBTM
                                             WRK-OUT-AGENC2-ATTR-EBTM
                                             WRK-OUT-CONTA-ATTR-EBTM
                                             WRK-OUT-NUMERO-ATTR-EBTM
                                             WRK-OUT-FILIAL-ATTR-EBTM
                                             WRK-OUT-CONTROLE-ATTR-EBTM
                   GO                    TO  2250-99-FIM
               ELSE
                   PERFORM 2110-ACESSAR-DCOM4174
               END-IF

               IF  WRK-INP-OPER-EBTM       NOT NUMERIC         OR
                   WRK-INP-OPER-EBTM       NOT GREATER ZEROS   OR
                   WRK-INP-NSEQ-EBTM       NOT NUMERIC         OR
                   WRK-INP-NSEQ-EBTM       NOT GREATER ZEROS
                   MOVE WRK-49369           TO WRK-OUT-OPER-ATTR-EBTM
                                               WRK-OUT-NSEQ-ATTR-EBTM
                   MOVE SG-MENSAGEM(83)     TO WRK-OUT-MENSA-EBTM
                   MOVE    '*'              TO WRK-INCONSIS
               ELSE
                   INITIALIZE COMU-AREA
                   MOVE '2'                 TO  WRK-TIPO-SELECAO
                   MOVE WRK-INP-OPER-EBTM   TO WRK-OUT-OPER-EBTM
                   MOVE WRK-INP-NSEQ-EBTM   TO WRK-OUT-NSEQ-EBTM
                   MOVE WRK-INP-OPER-EBTM   TO COMU-DANO
                   MOVE WRK-INP-NSEQ-EBTM   TO COMU-NSEQ
               END-IF
           END-IF.

           IF  WRK-TIPO-SELECAO        EQUAL '2' AND
               WRK-INCONSIS            NOT EQUAL '*'
               PERFORM  2290-VERIFICA-SITUACAO
           END-IF

           IF  WRK-INCONSIS             EQUAL '*'
               GO                       TO  2250-99-FIM
           END-IF.


           IF  WRK-INP-AGENCIA-EBTM    NOT EQUAL  SPACES
               IF  WRK-INP-AGENC2-EBTM   NOT EQUAL  SPACES  OR
                   WRK-INP-CONTA-EBTM    NOT EQUAL  SPACES  OR
                   WRK-INP-CPFCNPJ-EBTM  NOT EQUAL  SPACES
                   MOVE WRK-MSG-ERRO01   TO  WRK-OUT-MENSA-EBTM
                   MOVE '*'              TO  WRK-INCONSIS
                   MOVE WRK-49369        TO  WRK-OUT-AGENCIA-ATTR-EBTM
                                             WRK-OUT-AGENC2-ATTR-EBTM
                                             WRK-OUT-CONTA-ATTR-EBTM
                                             WRK-OUT-NUMERO-ATTR-EBTM
                                             WRK-OUT-FILIAL-ATTR-EBTM
                                             WRK-OUT-CONTROLE-ATTR-EBTM
                                             WRK-OUT-OPER-ATTR-EBTM
                                             WRK-OUT-NSEQ-ATTR-EBTM
                   GO                    TO  2250-99-FIM
               END-IF
               IF  WRK-INP-AGENCIA-EBTM    NOT NUMERIC         OR
                   WRK-INP-AGENCIA-EBTM    NOT GREATER ZEROS
                   MOVE WRK-49369          TO WRK-OUT-AGENCIA-ATTR-EBTM
                   MOVE SG-MENSAGEM(57)    TO WRK-OUT-MENSA-EBTM
                   MOVE '*'                TO WRK-INCONSIS
               ELSE
                   MOVE '1'                TO WRK-TIPO-SELECAO
                   INITIALIZE COMU-AREA
                   MOVE WRK-INP-AGENCIA-EBTM
                                           TO WRK-OUT-AGENCIA-EBTM
                                              COMU-AGENCIA
                                              WRK-AGENCIA-TELA
                                              ROTENT-6437-AGEPESQ-ENV
                   PERFORM 2260-ACESSAR-DCOM6437
               END-IF
           END-IF.

           IF  WRK-INCONSIS             EQUAL '*'
               GO                       TO  2250-99-FIM
           END-IF.

           IF (WRK-INP-NUMERO-EBTM      NOT EQUAL SPACES) OR
              (WRK-INP-FILIAL-EBTM      NOT EQUAL SPACES) OR
              (WRK-INP-CONTROLE-EBTM    NOT EQUAL SPACES)
              IF  WRK-INP-AGENC2-EBTM   NOT EQUAL  SPACES  OR
                  WRK-INP-CONTA-EBTM    NOT EQUAL  SPACES
                  MOVE WRK-MSG-ERRO01   TO  WRK-OUT-MENSA-EBTM
                  MOVE '*'              TO  WRK-INCONSIS
                  MOVE WRK-49369        TO  WRK-OUT-NUMERO-ATTR-EBTM
                                            WRK-OUT-FILIAL-ATTR-EBTM
                                            WRK-OUT-CONTROLE-ATTR-EBTM
                                            WRK-OUT-OPER-ATTR-EBTM
                                            WRK-OUT-NSEQ-ATTR-EBTM
                                            WRK-OUT-AGENCIA-ATTR-EBTM
                                            WRK-OUT-AGENC2-ATTR-EBTM
                                            WRK-OUT-CONTA-ATTR-EBTM
                  GO                    TO  2250-99-FIM
              END-IF
              PERFORM 2254-CONSISTE-CPF-CNPJ
           END-IF.

           IF  WRK-INCONSIS             EQUAL '*'
               GO                       TO  2250-99-FIM
           END-IF.

           IF (WRK-INP-AGENC2-EBTM     NOT EQUAL SPACES)  OR
              (WRK-INP-CONTA-EBTM      NOT EQUAL SPACES)
              PERFORM 2253-CONSISTE-AGENC-CTA
           END-IF.

121208     IF  WRK-INCONSIS             EQUAL SPACES
121208         PERFORM 2256-VER-DISPONIBILIDADE
121208
121208         IF  WRK-INCONSIS         EQUAL SPACES
                   PERFORM 2255-PASSAR-CONTROLE
121208         END-IF
121208     END-IF.

      *----------------------------------------------------------------*
       2250-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-ACESSAR-DCOM4174          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO ERRO-AREA WRK-SQLCA.

           INITIALIZE 4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR
                      ERRO-AREA.

           MOVE WRK-COD-USER-R         TO 4174-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM.
           MOVE 'N'                    TO 4174-ENT-FLAG.
           MOVE WRK-INP-OPER-EBTM      TO 4174-ENT-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ-EBTM      TO 4174-ENT-NSEQ-OPER-DESC.
           MOVE 'DCOM4174'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  4174-SAI-COD-RETORNO
               WHEN '0000'
                     MOVE 4174-SAI-CAG-BCRIA
                                       TO ROTENT-6437-AGEPESQ-ENV
                     PERFORM 2260-ACESSAR-DCOM6437
                     IF WRK-INCONSIS   EQUAL '*'
                        MOVE 4174-SAI-CAG-OPER-OPER
                                       TO ROTENT-6437-AGEPESQ-ENV
                        PERFORM 2260-ACESSAR-DCOM6437
                        IF WRK-INCONSIS   EQUAL '*'
                           MOVE WRK-49369 TO WRK-OUT-OPER-ATTR-EBTM
                                             WRK-OUT-NSEQ-ATTR-EBTM
                        END-IF
                     END-IF

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE 4174-SAI-MSG-RETORNO
                                       TO WRK-OUT-MENSA-EBTM
                     MOVE '*'          TO WRK-INCONSIS
                     MOVE WRK-49369    TO WRK-OUT-OPER-ATTR-EBTM
                                          WRK-OUT-NSEQ-ATTR-EBTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2253-CONSISTE-AGENC-CTA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA

           IF WRK-INP-AGENC2-EBTM     NOT NUMERIC         OR
              WRK-INP-AGENC2-EBTM     NOT GREATER ZEROS
              MOVE WRK-49369           TO WRK-OUT-AGENC2-ATTR-EBTM
                                          WRK-OUT-CONTA-ATTR-EBTM
              MOVE SG-MENSAGEM(057)
                                       TO WRK-OUT-MENSA-EBTM
              MOVE '*'                 TO WRK-INCONSIS
           ELSE
              MOVE '3'                 TO WRK-TIPO-SELECAO
              MOVE WRK-INP-AGENC2-EBTM TO WRK-OUT-AGENC2-EBTM
                                          COMU-AGENCIA
                                          WRK-AGENCIA-TELA
           END-IF.

           IF  WRK-INCONSIS            EQUAL '*'
               GO                      TO  2253-99-FIM
           END-IF.

           IF WRK-INP-CONTA-EBTM       NOT NUMERIC         OR
              WRK-INP-CONTA-EBTM       NOT GREATER ZEROS
              MOVE WRK-49369           TO  WRK-OUT-CONTA-ATTR-EBTM
              MOVE SG-MENSAGEM(058)
                                       TO  WRK-OUT-MENSA-EBTM
              MOVE '*'                 TO  WRK-INCONSIS
              GO                       TO  2253-99-FIM
           ELSE
              MOVE '3'                 TO  WRK-TIPO-SELECAO
              MOVE WRK-INP-CONTA-EBTM  TO  WRK-OUT-CONTA-EBTM
                                           COMU-CONTA
              PERFORM 2257-ACESSAR-DCOM6408
              IF  WRK-INCONSIS         EQUAL '*'
                  MOVE WRK-49369       TO WRK-OUT-AGENC2-ATTR-EBTM
                                          WRK-OUT-CONTA-ATTR-EBTM
                  GO                   TO 2253-99-FIM
              END-IF
              MOVE WRK-INP-AGENC2-EBTM-N TO ROTENT-6437-AGEPESQ-ENV
              PERFORM 2260-ACESSAR-DCOM6437
              IF  WRK-INCONSIS         EQUAL '*'
                  MOVE WRK-49369       TO WRK-OUT-AGENC2-ATTR-EBTM
                                          WRK-OUT-CONTA-ATTR-EBTM
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2253-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2254-CONSISTE-CPF-CNPJ          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA

           PERFORM 2210-1-EFETUAR-INSPECT.

           MOVE WRK-INP-NUMERO-EBTM    TO WRK-OUT-NUMERO-EBTM
                                          COMU-CNPJCPF
                                          WRK-NUMERO.

           MOVE WRK-INP-CONTROLE-EBTM  TO WRK-OUT-CONTROLE-EBTM
                                          COMU-CONTROLE
                                          WRK-CONTROLE.

           IF  WRK-INP-FILIAL-EBTM     EQUAL SPACES OR LOW-VALUES
                MOVE SPACES             TO WRK-FILIAL
                                          COMU-FILIAL
           ELSE
               MOVE WRK-INP-FILIAL-EBTM-N
                                       TO WRK-FILIAL
                                          WRK-OUT-FILIAL-EBTM
                                          COMU-FILIAL
           END-IF.

FS2511*     CALL 'BRAD0110'          USING  WRK-CPFCGC
FS2511*                                         WRK-CONTROLE
FS2511      CALL 'BRAD2000'          USING  WRK-CPFCGC
                                           WRK-CONTROLE

           IF  RETURN-CODE      NOT EQUAL  ZEROS
               MOVE WRK-49369      TO WRK-OUT-NUMERO-ATTR-EBTM
                                      WRK-OUT-FILIAL-ATTR-EBTM
                                      WRK-OUT-CONTROLE-ATTR-EBTM
               MOVE SG-MENSAGEM(05)
                                   TO WRK-OUT-MENSA-EBTM
               MOVE '*'            TO WRK-INCONSIS
               GO                  TO 2254-99-FIM
           ELSE
               IF  WRK-INP-CONTROLE-EBTM NOT EQUAL  WRK-CONTROLE
                   MOVE WRK-49369      TO WRK-OUT-NUMERO-ATTR-EBTM
                                          WRK-OUT-FILIAL-ATTR-EBTM
                                          WRK-OUT-CONTROLE-ATTR-EBTM
                   MOVE SG-MENSAGEM(05)
                                       TO WRK-OUT-MENSA-EBTM
                   MOVE '*'            TO WRK-INCONSIS
                   GO                  TO 2254-99-FIM
               END-IF
           END-IF.

           MOVE '4'                TO WRK-TIPO-SELECAO.
           PERFORM 2257-ACESSAR-DCOM6408.

           IF  WRK-INCONSIS        EQUAL '*'
               MOVE WRK-49369      TO WRK-OUT-NUMERO-ATTR-EBTM
                                      WRK-OUT-FILIAL-ATTR-EBTM
                                      WRK-OUT-CONTROLE-ATTR-EBTM
           END-IF.

      *----------------------------------------------------------------*
       2254-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2255-PASSAR-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG               TO WRK-FUNCAO.

           IF WRK-INP-OPCAO-EBTM       EQUAL '1'
              EVALUATE    WRK-TIPO-SELECAO
               WHEN    '1'
                   MOVE  LENGTH OF COMU-AREA     TO  COMU-LL
                   MOVE  WRK-INP-AGENCIA-EBTM-N  TO  COMU-AGENCIA
                   MOVE  WRK-INP-TIMESTAMP-EBTM  TO  COMU-TIMESTAMP
                   MOVE  WRK-INP-OPCAO-EBTM      TO  COMU-TPOPCAO
                   MOVE  'DCOM0502'              TO  COMU-TRANSACAO
                   MOVE  COMU-AREA               TO  WRK-MENSAGEM
                   MOVE  'DCOM0503'              TO  WRK-TELA
               WHEN    '2'

                   MOVE  LENGTH OF COMU-AREA     TO  COMU-LL
                   MOVE  WRK-INP-TIMESTAMP-EBTM  TO  COMU-TIMESTAMP
                   MOVE  'DCOM0502'              TO  COMU-TRANSACAO
                   MOVE  WRK-INP-OPER-EBTM-N     TO  COMU-DANO
                   MOVE  WRK-INP-NSEQ-EBTM-N     TO  COMU-NSEQ
                   MOVE  WRK-INP-OPCAO-EBTM      TO  COMU-TPOPCAO
                   MOVE  'DCOM0505'              TO  WRK-TELA
                   MOVE  COMU-AREA               TO  WRK-MENSAGEM

               WHEN    OTHER
                   MOVE  LENGTH OF COMU-AREA     TO  COMU-LL
                   MOVE  WRK-INP-TIMESTAMP-EBTM  TO  COMU-TIMESTAMP
                   MOVE  WRK-INP-OPCAO-EBTM      TO  COMU-TPOPCAO
                   MOVE  'DCOM0502'              TO  COMU-TRANSACAO
                   MOVE  COMU-AREA               TO  WRK-MENSAGEM
                   MOVE  'DCOM0504'              TO  WRK-TELA
              END-EVALUATE
           END-IF.

           IF WRK-INP-OPCAO-EBTM       EQUAL '2'
              EVALUATE    WRK-TIPO-SELECAO
               WHEN    '1'
                   MOVE  LENGTH OF COMU-AREA     TO  COMU-LL
                   MOVE  WRK-INP-AGENCIA-EBTM-N  TO  COMU-AGENCIA
                   MOVE  WRK-INP-TIMESTAMP-EBTM  TO  COMU-TIMESTAMP
                   MOVE  WRK-INP-OPCAO-EBTM      TO  COMU-TPOPCAO
                   MOVE  'DCOM0502'              TO  COMU-TRANSACAO
                   MOVE  COMU-AREA               TO  WRK-MENSAGEM
                   MOVE  'DCOM0503'              TO  WRK-TELA
               WHEN    '2'

                   MOVE  LENGTH OF COMU-AREA     TO  COMU-LL
                   MOVE  WRK-INP-TIMESTAMP-EBTM  TO  COMU-TIMESTAMP
                   MOVE  WRK-INP-OPCAO-EBTM      TO  COMU-TPOPCAO
                   MOVE  'DCOM0502'              TO  COMU-TRANSACAO
                   MOVE  WRK-INP-OPER-EBTM-N     TO  COMU-DANO
                   MOVE  WRK-INP-NSEQ-EBTM-N     TO  COMU-NSEQ
                   MOVE  'DCOM0506'              TO  WRK-TELA
                   MOVE  COMU-AREA               TO  WRK-MENSAGEM

               WHEN    OTHER
                   MOVE  LENGTH OF COMU-AREA     TO  COMU-LL
                   MOVE  WRK-INP-TIMESTAMP-EBTM  TO  COMU-TIMESTAMP
                   MOVE  WRK-INP-OPCAO-EBTM      TO  COMU-TPOPCAO
                   MOVE  'DCOM0502'              TO  COMU-TRANSACAO
                   MOVE  COMU-AREA               TO  WRK-MENSAGEM
                   MOVE  'DCOM0504'              TO  WRK-TELA
              END-EVALUATE
           END-IF.

           IF WRK-INP-OPCAO-EBTM       EQUAL '3'
              EVALUATE    WRK-TIPO-SELECAO
               WHEN    '1'
                   MOVE  LENGTH OF COMU-AREA     TO  COMU-LL
                   MOVE  WRK-INP-AGENCIA-EBTM-N  TO  COMU-AGENCIA
                   MOVE  WRK-INP-TIMESTAMP-EBTM  TO  COMU-TIMESTAMP
                   MOVE  WRK-INP-OPCAO-EBTM      TO  COMU-TPOPCAO
                   MOVE  'DCOM0502'              TO  COMU-TRANSACAO
                   MOVE  COMU-AREA               TO  WRK-MENSAGEM
                   MOVE  'DCOM0503'              TO  WRK-TELA
               WHEN    '2'

                   MOVE  LENGTH OF COMU-AREA TO  COMU-LL
                   MOVE  WRK-INP-TIMESTAMP-EBTM
                                                 TO  COMU-TIMESTAMP
                   MOVE  'DCOM0502'              TO  COMU-TRANSACAO
                   MOVE  WRK-INP-OPCAO-EBTM      TO  COMU-TPOPCAO
                   MOVE  WRK-INP-OPER-EBTM-N     TO COMU-DANO
                   MOVE  WRK-INP-NSEQ-EBTM-N     TO COMU-NSEQ
                   MOVE  'DCOM0507'              TO  WRK-TELA
                   MOVE  COMU-AREA               TO  WRK-MENSAGEM

               WHEN    OTHER
                   MOVE  LENGTH OF COMU-AREA
                                                 TO  COMU-LL
                   MOVE  WRK-INP-TIMESTAMP-EBTM
                                                 TO  COMU-TIMESTAMP
                   MOVE  WRK-INP-OPCAO-EBTM      TO  COMU-TPOPCAO
                   MOVE  'DCOM0502'              TO  COMU-TRANSACAO
                   MOVE  COMU-AREA               TO  WRK-MENSAGEM
                   MOVE  'DCOM0504'              TO  WRK-TELA
              END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
       2255-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2256-VER-DISPONIBILIDADE        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6438-ENVIO.

           MOVE 'N'                    TO ROTENT-6438-FLAG.
           MOVE WRK-COD-USER           TO ROTENT-6438-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6438-CTERM.
           MOVE 2                      TO ROTENT-6438-AGEUSU-ENV.
           MOVE ZEROS                  TO ROTENT-6438-PROD-ENV.
           MOVE 'M'                    TO ROTENT-6438-PROC-ENV.

           EVALUATE  WRK-INP-OPCAO-EBTM
               WHEN '1'
                     MOVE 'I'          TO ROTENT-6438-FUNC-ENV

               WHEN '2'
                     MOVE 'E'          TO ROTENT-6438-FUNC-ENV

               WHEN '3'
                     MOVE 'C'          TO ROTENT-6438-FUNC-ENV
           END-EVALUATE.

      *--> ESTORNO DA OPERACAO
           MOVE 010                    TO ROTENT-6438-OPCAO-ENV
           MOVE 'OP'                   TO ROTENT-6438-MOT-ENV.
           MOVE 'DCOM6438'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6438-ENVIO
                                             ROTSAI-6438-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6438-COD-RET     EQUAL '0099'
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  ROTSAI-6438-COD-RET     EQUAL ZEROS AND
               ROTSAI-6438-RESUL-RET   EQUAL 1
               NEXT SENTENCE
           ELSE
               MOVE '*'                TO WRK-INCONSIS
               MOVE ROTSAI-6438-MENS-RET
                                       TO WRK-OUT-MENSA-EBTM
           END-IF.

      *----------------------------------------------------------------*
       2256-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2257-ACESSAR-DCOM6408           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                      TO WRK-SQLCA ERRO-AREA.

           INITIALIZE ROTENT-6408-AREA
                      ROTSAI-6408-RETORNO
                      ERRO-AREA.

           IF  WRK-TIPO-SELECAO             EQUAL '3'
               MOVE WRK-INP-AGENC2-EBTM-N   TO ROTENT-6408-CJUNC-ENV
               MOVE WRK-INP-CONTA-EBTM-N    TO ROTENT-6408-CCTAC-ENV
               PERFORM 2258-OBTER-DIGITO-CONTA
               MOVE WRK-DIGITO-0431         TO ROTENT-6408-DCTAC-ENV
           ELSE
               MOVE WRK-INP-NUMERO-EBTM-N   TO ROTENT-6408-NUM-CNPJ-ENV
               MOVE WRK-INP-CONTROLE-EBTM-N TO ROTENT-6408-CTR-CNPJ-ENV
               IF WRK-INP-FILIAL-EBTM       NOT EQUAL SPACES
                 MOVE WRK-INP-FILIAL-EBTM-N TO ROTENT-6408-FIL-CNPJ-ENV
               END-IF
           END-IF.

           MOVE 'DCOM6408'                  TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6408-AREA
                                             ROTSAI-6408-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  ROTSAI-6408-COD-RET
               WHEN '0000'
                     CONTINUE

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE '*'            TO WRK-INCONSIS
                 IF  ROTSAI-6408-COD-RET EQUAL '0003'
                     IF WRK-TIPO-SELECAO EQUAL '3'
                        MOVE 'AGENCIA/CONTA INEXISTENTE'
                                         TO WRK-OUT-MENSA-EBTM
                     ELSE
                        MOVE 'CPF/CNPJ INEXISTENTE'
                                         TO WRK-OUT-MENSA-EBTM
                     END-IF
                 ELSE
                     MOVE ROTSAI-6408-MENS-RET
                                         TO WRK-OUT-MENSA-EBTM
                 END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       2257-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2258-OBTER-DIGITO-CONTA         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-CONTA-EBTM-N   TO WRK-CONTA-0431.
           MOVE 07                     TO WRK-TAMANHO-0431.
           MOVE SPACES                 TO WRK-DIGITO-0431.

           CALL 'BRAD0431'             USING WRK-CONTA-0431
                                             WRK-DIGITO-0431
                                             WRK-TAMANHO-0431.

           IF  WRK-DIGITO-0431         EQUAL '.'
               MOVE  'ERRO NO ACESSO A BRAD0431'
                                       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-DIGITO-0431     EQUAL 'P'
                   MOVE '0'            TO WRK-DIGITO-0431
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2258-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2260-ACESSAR-DCOM6437           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO ERRO-AREA WRK-SQLCA.
           MOVE WRK-COD-USER           TO ROTENT-6437-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6437-CTERM.
           MOVE WRK-COD-DEPTO-N        TO ROTENT-6437-AGEUSU-ENV.
           MOVE 'A'                    TO ROTENT-6437-FUNCAO-ENV.
           MOVE 'DCOM6437'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6437-ENVIO
                                             ROTSAI-6437-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET     NOT EQUAL ZEROS OR
               ROTSAI-6437-RESUL-RET   GREATER 3
               MOVE '*'                TO WRK-INCONSIS
               MOVE SPACES             TO WRK-OUT-MENSA-EBTM
               IF  ROTSAI-6437-RESUL-RET GREATER 3
                   MOVE 'USUARIO NAO AUTORIZADO'
                                       TO WRK-OUT-MENSA-EBTM
               ELSE
                   MOVE ROTSAI-6437-MENS-RET
                                       TO WRK-OUT-MENSA-EBTM
               END-IF
               IF  WRK-TIPO-SELECAO    EQUAL '1'
                   MOVE WRK-49369      TO WRK-OUT-AGENCIA-ATTR-EBTM
               ELSE
                   MOVE WRK-49369      TO WRK-OUT-AGENC2-ATTR-EBTM
                                          WRK-OUT-CONTA-ATTR-EBTM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2290-VERIFICA-SITUACAO          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO ERRO-AREA WRK-SQLCA.

           INITIALIZE ENTRADA-5426-ROTEADOR
                      SAIDA-5426-ROTEADOR
                      ERRO-AREA.

           MOVE WRK-COD-USER           TO RE5426-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO RE5426-CTERM
           MOVE WRK-INP-OPER-EBTM-N    TO RE5426-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-EBTM-N    TO RE5426-NSEQ-OPER-DESC
           MOVE 'ESTORNO'              TO RE5426-TIPO-OPERACAO

           EVALUATE  WRK-INP-OPCAO-EBTM
              WHEN '1'
                 MOVE 'I'              TO RE5426-TIPO-CONSULTA
              WHEN '2'
                 MOVE 'E'              TO RE5426-TIPO-CONSULTA
              WHEN '3'
                 MOVE 'C'              TO RE5426-TIPO-CONSULTA
           END-EVALUATE

           MOVE 'DCOM5426'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ENTRADA-5426-ROTEADOR
                                             SAIDA-5426-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  RS5426-COD-RETORNO      NOT EQUAL ZEROS
               MOVE  '*'                   TO WRK-INCONSIS
               MOVE  SPACES                TO WRK-OUT-MENSA-EBTM
               MOVE  WRK-49369             TO WRK-OUT-OPER-ATTR-EBTM
                                              WRK-OUT-NSEQ-ATTR-EBTM
               MOVE  RS5426-MSG-RETORNO    TO WRK-OUT-MENSA-EBTM
           END-IF.

      *----------------------------------------------------------------*
       2290-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2299-VERIFICA-PERFIL            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO ERRO-AREA WRK-SQLCA.

           INITIALIZE ENTRADA-5425-ROTEADOR
                      SAIDA-5425-ROTEADOR
                      ERRO-AREA.

           MOVE WRK-COD-USER           TO RE5425-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO RE5425-CTERM
           MOVE WRK-INP-OPER-EBTM-N    TO RE5425-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-EBTM-N    TO RE5425-NSEQ-OPER-DESC
           MOVE 'OPERACAO'             TO RE5425-TIPO-CONSULTA

           MOVE 'DCOM5425'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ENTRADA-5425-ROTEADOR
                                             SAIDA-5425-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  RS5425-COD-RETORNO      NOT EQUAL ZEROS
               MOVE '*'                TO WRK-INCONSIS
               MOVE SPACES             TO WRK-OUT-MENSA-EBTM
               MOVE WRK-49369          TO WRK-OUT-OPER-ATTR-EBTM
                                          WRK-OUT-NSEQ-ATTR-EBTM
               IF  RS5425-COD-RETORNO  EQUAL '3333'
                   MOVE 'USUARIO NAO AUTORIZADO'
                                       TO WRK-OUT-MENSA-EBTM
               ELSE
                   MOVE RS5425-MSG-RETORNO
                                       TO WRK-OUT-MENSA-EBTM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2299-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-EBTM
                                             WRK-660-DCOMEBTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-EBTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0502'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0502'         TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER           TO ERR-COD-USER
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO

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

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
