      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVEMS                                            *
      * DESCRICAO: BOOK DE COMUNICACAO COM MODULO EMSG1000             *
      *            ENVIO DE MENSAGEM DE APROVACAO DE LIMITE            *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : KAUE SANTOS                                         *
      * PROJETO  : PROPOSTA ELETRONICA FASE II PAGAMENTO A PRAZO       *
      * TAMANHO  : 01332 (LIMITADO A 28700)                            *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *                INFORMACOES DO SEGMENTO PRINCIPAL               *
      *----------------------------------------------------------------*
      * CAMPO                | DESCRICAO                               *
      *----------------------------------------------------------------*
      * SEGPRINC             | IDENTIFICADOR DO BLOCO DE DADOS         *
      *                      | SEGMENTO PRINCIPAL (EMSG00020)          *
      *                                                                *
      * TAMPRINC             | TAMANHO TOTAL DO BLOCO ENVIADO          *
      *                                                                *
      * VERPRINC             | VERSAO DO LAY OUT (001)                 *
      *                                                                *
      * APL-ORIG             | MOVER CENTRO DE CUSTO DA APLICACAO      *
      *                      | ORIGINADORA DO EVENTO DA MENSAGEM       *
      *                                                                *
      * APL-DEST             | MOVER O CENTRO DE CUSTO DO MODULO       *
      *                      | CHAMADOR. QUANDO A APLICACAO ORIGI-     *
      *                      | NADORA FOR A MESMA QUE A CHAMADORA,     *
      *                      | MOVER O MESMO CENTRO DE CUSTO           *
      *                                                                *
      * NOME-LOGICO          | NOME UTILIZADO PARA IDENTIFICAR A       *
      *                      | MENSAGEM A SER ENVIADA. ESTE DADO       *
      *                      | SERA INFORMADO PELO GESTOR DO EMSG      *
      *                      | NO MOMENTO DO CADASTRAMENTO DA          *
      *                      | DA MENSAGEM (BRANCO).                   *
      *                                                                *
      * COD-IDIOMA           | INFORMARA O CODIGO DO IDIOMA DO         *
      *                      | TEMPLATE DA MENSAGEM QUE SERA           *
      *                      | UTILIZADO NO MOMENTO DE FORMACAO DA     *
      *                      | MENSAGEM (001).                         *
      *                                                                *
      * DATA-ENVIO           | DATA QUE A MENSAGEM DEVERA              *
      *                      | SER ENVIADA NO FORMATO DD/MM/AAAA.      *
      *                      | CASO NAO PREENCHIDO O CAMPO, ASSUMIRA   *
      *                      | ENVIO IMEDIATO.                         *
      *                                                                *
      * HRINI-ENVIO          | HORA INICIAL PREVISTA PARA O ENVIO      *
      *                      | DE MENSAGEM (HORA DE BRASILIA NO        *
      *                      | FORMATO HH:MM:SS).                      *
      *                      | OBRIGATORIO SE CAMPO DATA AGENDADA      *
      *                      | ENVIO FOR PREENCHIDO.                   *
      *                                                                *
      * HRFIM-ENVIO          | HORA LIMITE PARA O ENVIO DE MENSAGEM    *
      *                      | (HORA DE BRASILIA NO FORMATO HH:MM:SS)  *
      *                      | NAO OBRIGATORIO.                        *
      *                                                                *
      * EMPRESA              | PARA UTILIZACAO NO CONCEITO MULTI-      *
      *                      | BANCO = FIXO '237' QUANDO BRADESCO      *
      *                                                                *
      * TIP-PESQ             | INFORMA TIPO DOS DADOS PARA IDENTI-     *
      *                      | FICACAO DO CLIENTE:                     *
      *                      | 001 - AGÊNCIA E CONTA                   *
      *                      | 002 - CPF                               *
      *                      | 003 - CNPJ                              *
      *                      | 004 - DEPTO / SEÇAO                     *
      *                                                                *
      * DADOS DE PESQUISA    | VINCULADO AO CAMPO TIPO IDENTIFICADOR   *
      *                      | DO CLIENTE:                             *
      *                      | 001 - INFORMAR AGENCIA E CONTA          *
      *                      | 002 - INFORMAR CPF                      *
      *                      | 003 - INFORMAR CNPJ                     *
      *                      | 004 - INFORMAR DEPTO / SEÇAO            *
      *                                                                *
      * TAM-AREA-LIV         | TAMANHO AREA LIVRE: REGRAS DE TARIFACAO *
      *                                                                *
      * AREA-LIVRE           | CAMPO LIVRE. A INFORMAÇAO PASSADA NESTA *
      *                      | AREA SERA ENTREGUE NO ARQUIVO DE LOG    *
      *                      | DIARIO GERADO PARA MODULO DE TARIFACAO  *
      *                                                                *
      *----------------------------------------------------------------*
      *         INFORMACOES DO SEGMENTO PARAMETROS DA MENSAGEM         *
      *----------------------------------------------------------------*
      * CAMPO                | DESCRICAO                               *
      *----------------------------------------------------------------*
      * SEGPARAM             | IDENTIFICADOR DO BLOCO DE DADOS         *
      *                      | SEGMENTO PARAMETRO (EMSGDICD)           *
      *                                                                *
      * TAMPARAM             | TAMANHO TOTAL DO BLOCO ENVIADO          *
      *                                                                *
      * VERPARAM             | VERSAO DO LAY OUT (001)                 *
      *                                                                *
      * COD-DICD             | CODIGO DA ESTRUTURA DE DADOS CRIADA     *
      *                      | NO ELEMENTO ESTRUTURAL DICD             *
      *                                                                *
      * VER-DICD             | VERSAO DA ESTRUTURA DE DADOS CRIADA     *
      *                      | NO ELEMENTO ESTRUTURAL DICD             *
      *                                                                *
      * TAM-DADOS            | TAMANHO TOTAL DA MENSAGEM               *
      *                                                                *
      * MENSAGEM             | DADOS DA MENSAGEM DE EMAIL A ENVIAR     *
      *                                                                *
      *----------------------------------------------------------------*
      *            INFORMACOES DO SEGMENTO DESTINO PRINCIPAL           *
      *----------------------------------------------------------------*
      * CAMPO                | DESCRICAO                               *
      *----------------------------------------------------------------*
      * SEGDESTI             | IDENTIFICADOR DO BLOCO DE DADOS         *
      *                      | SEGMENTO PARAMETRO (EMSGMPRC)           *
      *                                                                *
      * TAMDESTI             | TAMANHO TOTAL DO BLOCO ENVIADO          *
      *                                                                *
      * VERDESTI             | VERSAO DO LAY OUT (001)                 *
      *                                                                *
      * QTD-DEST-EMAIL       | QUANTIDADE DE DESTINATARIOS EMAIL       *
      *                      | PARA ENVIO DA MENSAGEM                  *
      *                                                                *
      * TIP-RECEP            | TIPO DO RECEPTOR                        *
      *                      |  1 - PARA                               *
      *                      |  2 - COPIA                              *
      *                      |  3 - COPIA OCULTA                       *
      *                                                                *
      * EMAIL                | ENDERECO DO EMAIL                       *
      *                                                                *
      * QTD-TOKEN            | QUANTIDADE TOKENS CADASTRADOS NO IMPD   *
      *                                                                *
      * TOKEN                | TOKEN IDENTIFICACAO NO IMPD             *
      *----------------------------------------------------------------*
       01  BVVEMS-AREA.
      *    SEGMENTO PRINCIPAL
           03 BVVEMS-SEGMENTO-PRINC.
              05 BVVEMS-SEGPRINC       PIC  X(008).
              05 BVVEMS-TAMPRINC       PIC  9(005).
              05 BVVEMS-VERPRINC       PIC  9(003).
              05 BVVEMS-FILLER01       PIC  X(040).
              05 BVVEMS-APL-ORIG       PIC  X(004).
              05 BVVEMS-APL-DEST       PIC  X(004).
              05 BVVEMS-NOME-LOGICO    PIC  X(080).
              05 BVVEMS-COD-IDIOMA     PIC  9(005).
              05 BVVEMS-DATA-ENVIO     PIC  X(010).
              05 BVVEMS-HRINI-ENVIO    PIC  X(008).
              05 BVVEMS-HRFIM-ENVIO    PIC  X(008).
              05 BVVEMS-EMPRESA        PIC  9(005).
              05 BVVEMS-TIP-PESQ       PIC  9(003).
                 88 BVVEMS-TIPESQ-AGCC   VALUE  1.
                 88 BVVEMS-TIPESQ-CPF    VALUE  2.
                 88 BVVEMS-TIPESQ-CNPJ   VALUE  3.
                 88 BVVEMS-TIPESQ-DEPTO  VALUE  4.
              05 BVVEMS-DADOS-PESQUISA PIC  X(128)    VALUE  SPACES.
              05 FILLER   REDEFINES  BVVEMS-DADOS-PESQUISA.
                 10 BVVEMS-AGENCIA     PIC  9(005).
                 10 BVVEMS-CONTA       PIC  9(013).
                 10 FILLER             PIC  X(110).
              05 FILLER   REDEFINES  BVVEMS-DADOS-PESQUISA.
                 10 BVVEMS-CPF-CNPJ    PIC  9(009).
                 10 BVVEMS-FILIAL      PIC  9(004).
                 10 BVVEMS-CONTROL     PIC  9(002).
                 10 BVVEMS-FILLER02    PIC  X(113).
              05 FILLER   REDEFINES  BVVEMS-DADOS-PESQUISA.
                 10 BVVEMS-DEPTMTO     PIC  9(005).
                 10 BVVEMS-SECAO       PIC  9(005).
                 10 FILLER             PIC  X(118).
              05 BVVEMS-TAM-AREA-LIV   PIC  9(003).
              05 BVVEMS-AREA-LIVRE     PIC  X(256).
      *    SEGMENTO PARAMETROS DA MENSAGEM
           03 BVVEMS-SEGMENTO-PARAM.
              05 BVVEMS-SEGPARAM       PIC  X(008).
              05 BVVEMS-TAMPARAM       PIC  9(005).
              05 BVVEMS-VERPARAM       PIC  9(003).
              05 BVVEMS-FILLER03       PIC  X(040).
              05 BVVEMS-COD-DICD       PIC  X(130).
              05 BVVEMS-VER-DICD       PIC  X(030).
              05 BVVEMS-TAM-DADOS      PIC  9(005).
              05 BVVEMS-MENSAGEM       PIC  X(120).
      *    SEGMENTO DESTINATARIO PRINCIPAL
           03 BVVEMS-SEGMENTO-DESTI.
              05 BVVEMS-SEGDESTI       PIC  X(008).
              05 BVVEMS-TAMDESTI       PIC  9(005).
              05 BVVEMS-VERDESTI       PIC  9(003).
              05 BVVEMS-FILLER04       PIC  X(064).
              05 BVVEMS-QTD-DEST-EMAIL PIC  9(005).
              05 BVVEMS-DADOS-EMAIL  OCCURS 2 TIMES.
                 10 BVVEMS-TIPO-RECEP  PIC  9(001).
                 10 BVVEMS-EMAIL       PIC  X(150).
              05 BVVEMS-QTD-TOKEN      PIC  9(002).
              05 BVVEMS-TOKEN          PIC  9(015).
