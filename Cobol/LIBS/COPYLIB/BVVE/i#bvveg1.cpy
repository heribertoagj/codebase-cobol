      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : REGISTROS-DETALHE DO  ARQUIVO DE CONFIRMACOES DE    *
      *            DOMICILIO BANCARIO ACRESCIDOS DE DADOS DO HEADER    *
      *            E FLAGS DE INCONSISTENCIA                           *
      *  LRECL   : 054 (FB)                                            *
      *  NOME INC: I#BVVEG1                                            *
      *  DATA    : 01/08/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEG1-REG.
           05  BVVEG1-CNPJCPF-NRO             PIC 9(009)     COMP-3.
           05  BVVEG1-CNPJ-FILIAL             PIC 9(005)     COMP-3.
           05  BVVEG1-CNPJCPF-CTRL            PIC 9(002).
           05  BVVEG1-PONTO-VENDA             PIC 9(009)     COMP-3.
           05  BVVEG1-AGENCIA                 PIC 9(005)     COMP-3.
           05  BVVEG1-CONTA-NRO               PIC 9(013)     COMP-3.
           05  BVVEG1-CONTA-DIG               PIC X(001).
           05  BVVEG1-TIPO-ALTERACAO          PIC 9(001).
           05  BVVEG1-DTSOLIC-ALTERA          PIC 9(009)     COMP-3.
           05  BVVEG1-NRO-SEQUENCIAL          PIC 9(005)     COMP-3.
           05  BVVEG1-DATA-CTRL               PIC 9(009)     COMP-3.
           05  BVVEG1-VERSAO-ARQ              PIC 9(009)     COMP-3.
           05  BVVEG1-BANCO-ENVIO             PIC 9(003)     COMP-3.
           05  BVVEG1-FLAG-BANCO              PIC 9(001).
           05  BVVEG1-FLAG-CTAINEX            PIC 9(001).
           05  BVVEG1-FLAG-CNPJIGCTA          PIC 9(001).
           05  BVVEG1-FLAG-CNPJRADIC          PIC 9(001).
           05  BVVEG1-FLAG-SCCE               PIC 9(001).
           05  BVVEG1-FLAG-BLOQ               PIC 9(001).
           05  BVVEG1-FLAG-CCMS               PIC 9(001).
      *----------------------------------------------------------------*
      * 001-005 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 006-008 FILIAL DO CNPJ DO ESTABELECIMENTO
      * 009-010 CONTROLE DO CNPJ/CPF DO ESTABELECIMENTO
      * 011-015 NUMERO DO PONTO DE VENDA DO ESTABELECIMENTO
      * 016-018 AGENCIA DA CONTA CORRENTE DO ESTABELECIMENTO(SEM DIGITO)
      * 019-025 NUMERO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 026-026 DIGITO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 027-027 TIPO DE ALTERACAO(1=GE 2=WF 3=LEGADO)
      * 028-032 DATA DA SOLICITACAO DA ALTERACAO (AAAAMMDD)
      * 033-035 NUMERO SEQUENCIAL DE CONTROLE
      * 036-040 DATA DE ENVIO PARA CONTROLE DE CLASSIFICACAO
      * 041-045 VERSAO DO ARQUIVO
      * 046-047 CODIGO DO BANCO DE ENVIO
      * 048-048 INDICATIVO DE BANCO INVALIDO (<> 237)
      * 049-049 INDICATIVO DE CONTA CORRENTE INEXISTENTE
      * 050-050 INDICATIVO DE CNPJ IGUAL AO DA CONTA CORRENTE
      * 051-051 INDICATIVO DE RADICAL DO CNPJ IGUAL AO DA CONTA CORRENTE
      * 052-052 INDICATIVO CONTA CORRENTE ENCERRADA SEGUNDO ROTINA SCCE
      * 053-053 INDICATIVO CONTA CORRENTE COM BLOQUEIO
      * 054-054 INDICATIVO CONTA INATIVA/ENCERRADA SEGUNDO ROTINA CCMS
