******************************************************************
      * SISTEMA : DCOM - I#DCOMQ6                                      *
      * TAMANHO : 0550 BYTES                                           *
      * ARQUIVO : OPERACAO DESCONTO COMERCIAL                          *
      * INCLUDE DA TABELA DCOMB0C5 - DB2PRD.TOPER_DESC_COML            *
      ******************************************************************
DTS001* DATA.....: 09/12/2011                                          *
DTS001* OBJETIVO.: INCLUIR CAMPO Q6-C5-TPO-COMCZ-RURAL. ESTE CAMPO SERA*
DTS001*   FORMATADO PARA NPR, POIS E O TIPO DE COMERCIALIZACAO RURAL.  *
DTS001******************************************************************
FS2511* MIGRACAO  : 11/2025                                            *
FS2511* OBJETIVO  : Adequacao dos campos de pessoa (CCNPJ/CPF e FILIAL)*
FS2511*             para formato alfanumerico, mantendo compatibilidade*
FS2511*             com interfaces existentes e regras de negocio.     *
FS2511*
FS2511* ALTERACOES:                                                     *
FS2511*  - Q6-C5-CCNPJ-CPF   alterado de PIC S9(9)V COMP-3 para PIC X(9)
FS2511*  - Q6-C5-CFLIAL-CNPJ alterado de PIC S9(5)V COMP-3 para PIC X(4)
FS2511*  - Q6-C5-CCTRL-CNPJ-CPF permanece numerico (PIC S9(2)V COMP-3)
FS2511*                                                             
FS2511* REGRAS IMPORTANTES:                                            *
FS2511*  - FILIAL sempre ocupa 4 caracteres. Para Pessoa Fisica (CPF), *
FS2511*    utilizar '0000'. Para Pessoa Juridica (CNPJ), utilizar a    *
FS2511*    filial correspondente (nao '0000').                         *
FS2511*  - CCNPJ/CPF e FILIAL sao armazenados e trafegados como texto  *
FS2511*    com zeros a esquerda. Evitar operacoes aritmeticas diretamente
FS2511*    sobre estes campos.                                         *
FS2511*  - Em comparacoes, "EQUAL ZEROS" com PIC X(4)/(9) continua     *
FS2511*    valido (todos os caracteres '0').                           *
FS2511*  - Em acesso a DB2 onde as colunas sao DECIMAL (ex.: CCNPJ_CPF*
FS2511*    DECIMAL(9,0), CFLIAL_CNPJ DECIMAL(5,0)), utilizar uma das   *
FS2511*    abordagens abaixo:                                          *
FS2511*      a) Criar variaveis host numericas (COMP-3) e mover os     *
FS2511*         valores textuais apos validacao para uso no SQL.       *
FS2511*      b) Usar CAST em SQL quando suportado, por exemplo:        *
FS2511*         WHERE CCNPJ_CPF = CAST(:Q6-C5-CCNPJ-CPF AS DECIMAL(9,0))
FS2511*           AND CFLIAL_CNPJ = CAST(:Q6-C5-CFLIAL-CNPJ AS DECIMAL(5,0))
FS2511*      c) Quando existir colunas ST (CHAR) como CCNPJ_CPF_ST e   *
FS2511*         CFLIAL_CNPJ_ST, preferir comparacao direta com host    *
FS2511*         alfanumerico.                                          *
FS2511*  - Garantir que os campos alfanumericos estejam preenchidos    *
FS2511*    apenas com digitos (0-9) antes de mover para variaveis      *
FS2511*    numericas ou enviar ao DB2.                                 *
FS2511*                                                             
FS2511* COMPATIBILIDADE:                                               *
FS2511*  - As interfaces que esperam CCNPJ/CPF e FILIAL concatenados   *
FS2511*    em tela continuam funcionando, pois os campos permanecem    *
FS2511*    como texto com zeros a esquerda.                            *
FS2511*  - A determinacao de Q6-C5-TIPO-PSSOA pode ser mantida via     *
FS2511*    teste de FILIAL EQUAL ZEROS ('0000' indica PF).             *
FS2511******************************************************************
       01  Q6-C5-REGISTRO.
 01 03     05 Q6-C5-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
 04 05     05 Q6-C5-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
 09 02     05 Q6-C5-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
 11 26     05 Q6-C5-HSIT-DESC-COML       PIC X(26).
 37 09     05 Q6-C5-VLIBRC-DESC-COML     PIC S9(15)V9(2) USAGE COMP-3.
 46 09     05 Q6-C5-VLIBRC-ANTCP-OPER    PIC S9(15)V9(2) USAGE COMP-3.
 55 02     05 Q6-C5-CBCO                 PIC S9(3)V USAGE COMP-3.
 57 03     05 Q6-C5-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
 60 07     05 Q6-C5-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
 67 02     05 Q6-C5-CBCO-OPER-OPER       PIC S9(3)V USAGE COMP-3.
 69 03     05 Q6-C5-CAG-OPER-OPER        PIC S9(5)V USAGE COMP-3.
 72 02     05 Q6-C5-CPRODT               PIC S9(3)V USAGE COMP-3.
 74 02     05 Q6-C5-CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
 76 02     05 Q6-C5-CTPO-DESC-COML       PIC S9(3)V USAGE COMP-3.
 78 05     05 Q6-C5-CCONTR-CONVE-DESC    PIC S9(9)V USAGE COMP-3.
 83 02     05 Q6-C5-CVRSAO-CONTR-CONVE   PIC S9(3)V USAGE COMP-3.
 85 02     05 Q6-C5-CELMTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
 87 05     05 Q6-C5-NSMULA-OPER-DESC     PIC S9(9)V USAGE COMP-3.
 92 01     05 Q6-C5-CINDCD-DIGTC-AG      PIC X(1).
 93 01     05 Q6-C5-CINDCD-ISENC-OPER    PIC X(1).
 94 01     05 Q6-C5-CINDCD-AUTRZ-OPER    PIC X(1).
 95 01     05 Q6-C5-CINDCD-MODLD-INCL    PIC X(1).
 96 04     05 Q6-C5-CCTRO-CUSTO          PIC X(4).
100 03     05 Q6-C5-DANO-BASE            PIC S9(4)V USAGE COMP-3.
103 03     05 Q6-C5-CDEPDC               PIC S9(5)V USAGE COMP-3.
106 05     05 Q6-C5-CSEQ-STUDO           PIC S9(9)V USAGE COMP-3.
111 01     05 Q6-C5-CINDCD-APROV-CREDT   PIC X(1).
112 09     05 Q6-C5-VOPER-DESC-COML      PIC S9(15)V9(2) USAGE COMP-3.
121 10     05 Q6-C5-DINIC-OPER-DESC      PIC X(10).
131 10     05 Q6-C5-DVCTO-FNAL-OPER      PIC X(10).
141 03     05 Q6-C5-TOPER-DESC-COML      PIC S9(5)V USAGE COMP-3.
144 02     05 Q6-C5-TMED-PONDE-OPER      PIC S9(3)V USAGE COMP-3.
146 02     05 Q6-C5-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
148 03     05 Q6-C5-QPCELA-OPER          PIC S9(5)V USAGE COMP-3.
151 05     05 Q6-C5-QTITLO-CHEQ-PCELA    PIC S9(9)V USAGE COMP-3.
156 04     05 Q6-C5-NAUTRZ-TRNSM-ESCR    PIC S9(7)V USAGE COMP-3.
160 01     05 Q6-C5-CINDCD-PGDOR-JURO    PIC X(1).
161 02     05 Q6-C5-CTPO-TX-JURO-DESC    PIC S9(3)V USAGE COMP-3.
163 04     05 Q6-C5-PMIN-TX-JURO-REGRA   PIC S9(3)V9(3) USAGE COMP-3.
167 04     05 Q6-C5-PMAX-TX-JURO-REGRA   PIC S9(3)V9(3) USAGE COMP-3.
171 05     05 Q6-C5-PALIQT-IOF-DESC      PIC S9(3)V9(5) USAGE COMP-3.
176 05     05 Q6-C5-PTX-JURO-MES         PIC S9(3)V9(5) USAGE COMP-3.
181 05     05 Q6-C5-PTX-JURO-ANO         PIC S9(3)V9(5) USAGE COMP-3.
186 05     05 Q6-C5-PTX-CUSTO-MES        PIC S9(3)V9(6) USAGE COMP-3.
191 05     05 Q6-C5-PTX-CUSTO-ANO        PIC S9(3)V9(6) USAGE COMP-3.
196 09     05 Q6-C5-VJURO-OPER-DESC      PIC S9(15)V9(2) USAGE COMP-3.
205 09     05 Q6-C5-VJURO-CUSTO-OPER     PIC S9(15)V9(2) USAGE COMP-3.
214 09     05 Q6-C5-VTARIF-REG-TITLO     PIC S9(15)V9(2) USAGE COMP-3.
223 09     05 Q6-C5-VTAC-OPER-DESC       PIC S9(15)V9(2) USAGE COMP-3.
232 09     05 Q6-C5-VPSENT-OPER-DESC     PIC S9(15)V9(2) USAGE COMP-3.
241 09     05 Q6-C5-VIOF-OPER-DESC       PIC S9(15)V9(2) USAGE COMP-3.
250 09     05 Q6-C5-VLIQ-OPER-DESC       PIC S9(15)V9(2) USAGE COMP-3.
259 01     05 Q6-C5-CINDCD-VALDC-ROTRO   PIC X(1).
260 10     05 Q6-C5-DVALDC-ROTRO-ELMTO   PIC X(10).
270 05     05 Q6-C5-CFUNC-VALDC-ROTRO    PIC S9(9)V USAGE COMP-3.
275 10     05 Q6-C5-DULT-ADTTO-OPER      PIC X(10).
285 01     05 Q6-C5-DULT-ADTTO-NULL      PIC X(01).
286 01     05 Q6-C5-CINDCD-MOTVO-ADTTO   PIC X(1).
287 01     05 Q6-C5-CINDCD-MOTVO-NULL    PIC X(01).
288 26     05 Q6-C5-HULT-ATULZ           PIC X(26).
314 05     05 Q6-C5-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
319 08     05 Q6-C5-CTERM                PIC X(8).
327 04     05 Q6-C5-CCONTR-CORP-OPER     PIC S9(7)V USAGE COMP-3.
331 10     05 Q6-C5-DINCL-REG            PIC X(10).
341 05     05 Q6-C5-CCART-LIM-CREDT      PIC X(5).
346 45     05 Q6-C5-CCHAVE-ELMTO-DESC    PIC X(45).
391 01     05 Q6-C5-CINDCD-ALIQT-REDZD   PIC X(1).
392 05     05 Q6-C5-PTX-JURO-DIA         PIC S9(3)V9(5) USAGE COMP-3.
397 02     05 Q6-C5-CINDCD-FREQ-TX       PIC X(2).
      ***                                                         ***
      ***  DADOS COMPLEMENTARES                                   ***
      ***                                                         ***
399 05     05 Q6-C5-CCART-NORML-DESC     PIC X(5).
404 05     05 Q6-C5-CCART-VENCD-DESC     PIC X(5).
409 05     05 Q6-C5-CCART-DESC-PRODT     PIC X(5).
FS2511*    05 Q6-C5-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
FS2511*    05 Q6-C5-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
FS2511     05 Q6-C5-CCNPJ-CPF            PIC X(09).
FS2511*    05 Q6-C5-CFLIAL-CNPJ          PIC X(05).
FS2511     05 Q6-C5-CFLIAL-CNPJ          PIC X(04).
FS2511     05 FILLER                     PIC X(01).
422 02     05 Q6-C5-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
424 60     05 Q6-C5-CEDENTE              PIC X(60).
484 01     05 Q6-C5-TIPO-PSSOA           PIC X(01).
485 05     05 Q6-C5-NPSSOA-DESC-COML     PIC S9(9)V USAGE COMP-3.
490 02     05 Q6-C5-CMEIO-PGTO-DESC      PIC S9(3)V USAGE COMP-3.
492 02     05 Q6-C5-CMEIO-PGTO-VENCD     PIC S9(3)V USAGE COMP-3.
494 01     05 Q6-C5-CINDCD-RESP-PGTO     PIC X(1).
495 02     05 Q6-C5-TDEB-PCELA-VENCD     PIC S9(3)V USAGE COMP-3.
497 02     05 Q6-C5-CINDCD-PRZ-PCELA     PIC X(2).
499 05     05 Q6-C5-CCART-CONE           PIC X(5).
504 01     05 Q6-C5-CORRENTISTA          PIC X(01).
505 01     05 Q6-C5-CINDCD-CALC-JURO     PIC 9(01).
      ***  TIPO: 1 - CALCULO LINEAR                              ***
      ***        2 - CALCULO EXPONENCIAL                         ***
506 02     05 Q6-C5-CTPO-GARNT           PIC S9(3)V USAGE COMP-3.
508 05     05 Q6-C5-PALIQT-IOF-DIA       PIC S9(3)V9(5) USAGE COMP-3.
513 01     05 Q6-C5-CINDCD-TX-ESPCL      PIC 9.
514 01     05 Q6-C5-TPO-COMCZ-RURAL      PIC X(1).
515 36     05 Q6-C5-FILLER               PIC X(36).
