      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : ARQUIVO DE  CONFIRMACOES DE DOMICILIO BANCARIO  A   *
      *            SER RECEBIDO, CONSISTIDO E RETORNADO 'A REDECARD.   *
      *  LRECL   : 080 (FB)                                            *
      *  NOME INC: I#BVVEG0                                            *
      *  DATA    : 27/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEG0-REG-HEADER.
           05  BVVEG0H-TIPO-REG-X.
               10  BVVEG0H-TIPO-REG           PIC 9(001).
           05  BVVEG0H-BANCO-ENVIO-X.
               10  BVVEG0H-BANCO-ENVIO        PIC 9(003).
           05  BVVEG0H-DATA-ENVIO-X.
               10  BVVEG0H-DATA-ENVIO         PIC 9(008).
           05  BVVEG0H-HORA-ENVIO-X.
               10  BVVEG0H-HORA-ENVIO         PIC 9(006).
           05  BVVEG0H-INDICATIVO-BASE        PIC X(001).
           05  BVVEG0H-RESERVADO1             PIC X(037).
           05  BVVEG0H-DATA-CTRL-X.
               10  BVVEG0H-DATA-CTRL          PIC 9(008).
           05  BVVEG0H-VERSAO-ARQ-X.
               10  BVVEG0H-VERSAO-ARQ         PIC 9(009).
           05  BVVEG0H-RESERVADO2             PIC X(007).
       01  BVVEG0-REG-DETALHE.
           05  BVVEG0D-TIPO-REG-X.
               10  BVVEG0D-TIPO-REG           PIC 9(001).
           05  BVVEG0D-BANCO-ENVIO-X.
               10  BVVEG0D-BANCO-ENVIO        PIC 9(003).
           05  BVVEG0D-PONTO-VENDA-X.
               10  BVVEG0D-PONTO-VENDA        PIC 9(009).
           05  BVVEG0D-NRO-SEQUENCIAL-X.
               10  BVVEG0D-NRO-SEQUENCIAL     PIC 9(005).
           05  BVVEG0D-CNPJ.
               10  BVVEG0D-CNPJ-NRO           PIC 9(008).
               10  BVVEG0D-CNPJ-FILIAL        PIC 9(004).
               10  BVVEG0D-CNPJ-CTRL          PIC 9(002).
           05  BVVEG0D-CPF REDEFINES BVVEG0D-CNPJ.
               10  BVVEG0D-RESERVADO1         PIC 9(003).
               10  BVVEG0D-CPF-NRO            PIC 9(009).
               10  BVVEG0D-CPF-CTRL           PIC 9(002).
           05  BVVEG0D-TIPO-PESSOA            PIC X(001).
           05  BVVEG0D-AGENCIA-X.
               10  BVVEG0D-AGENCIA            PIC 9(005).
           05  BVVEG0D-CONTA-NRO-X.
               10  BVVEG0D-CONTA-NRO          PIC 9(009).
           05  BVVEG0D-CONTA-DIG-X.
               10  BVVEG0D-CONTA-DIG          PIC 9(001).
           05  BVVEG0D-RESERVADO2             PIC X(005).
           05  BVVEG0D-COD-RETORNO-X.
               10  BVVEG0D-COD-RETORNO        PIC 9(002).
           05  BVVEG0D-TIPO-ALTERACAO-X.
               10  BVVEG0D-TIPO-ALTERACAO     PIC 9(001).
           05  BVVEG0D-DTSOLIC-ALTERA-X.
               10  BVVEG0D-DTSOLIC-ALTERA     PIC 9(008).
           05  BVVEG0D-DTRETOR-BANCO-X.
               10  BVVEG0D-DTRETOR-BANCO      PIC 9(008).
           05  BVVEG0D-TIPO-PROCES-X.
               10  BVVEG0D-TIPO-PROCES        PIC 9(001).
           05  BVVEG0D-RESERVADO3             PIC X(007).
       01  BVVEG0-REG-TRAILER.
           05  BVVEG0T-TIPO-REG-X.
               10  BVVEG0T-TIPO-REG           PIC 9(001).
           05  BVVEG0T-BANCO-ENVIO-X.
               10  BVVEG0T-BANCO-ENVIO        PIC 9(003).
           05  BVVEG0T-QTDE-REG-X.
               10  BVVEG0T-QTDE-REG           PIC 9(005).
           05  BVVEG0T-RESERVADO1             PIC X(047).
           05  BVVEG0T-DATA-CTRL-X.
               10  BVVEG0T-DATA-CTRL          PIC 9(008).
           05  BVVEG0T-RESERVADO2             PIC X(016).
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 0(ZERO) / HEADER
      * 002-004 CODIGO DO BANCO QUE ESTA RECEBENDO/ENVIANDO O ARQUIVO
      * 005-012 DATA DO ENVIO DO ARQUIVO (AAAAMMDD)
      * 013-018 HORARIO DO ENVIO DO ARQUIVO (HHMMSS)
      * 019-019 INDICATIVO BASE (L=LEGADO E=ELETRONICO)
      * 020-056 ESPACO VAGO/RESERVADO
      * 057-064 DATA DE ENVIO PARA CONTROLE DE CLASSIFICACAO
      * 065-073 VERSAO DO ARQUIVO
      * 074-080 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 1 (DETALHE)
      * 002-004 CODIGO DO BANCO QUE ESTA RECEBENDO/ENVIANDO O ARQUIVO
      * 005-013 NUMERO DO PONTO DE VENDA DO ESTABELECIMENTO
      * 014-018 NUMERO SEQUENCIAL DE CONTROLE
      * 019-032 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 033-033 TIPO DE PESSOA (F=FISICA J=JURIDICA)
      * 034-038 AGENCIA DA CONTA CORRENTE DO ESTABELECIMENTO(SEM DIGITO)
      * 039-047 NUMERO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 048-048 DIGITO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 049-053 RESERVA PARA USO FUTURO
      * 054-055 CODIGO DE RETORNO DO BANCO
      * 056-056 TIPO DE ALTERACAO(1=GE 2=WF 3=LEGADO)
      * 057-064 DATA DA SOLICITACAO DA ALTERACAO (AAAAMMDD)
      * 065-072 DATA DE RETORNO DO BANCO (AAAAMMDD)
      * 073-073 TIPO DE PROCESSO (1=CONFIRMACAO DE DOMICILIO)
      * 074-080 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
      * 001-001 TIPO DE REGISTRO = 9 (TRAILER)
      * 002-004 CODIGO DO BANCO QUE ESTA RECEBENDO/ENVIANDO O ARQUIVO
      * 005-009 QUANTIDADE DE REGISTROS (INCLUSIVE HEADER E TRAILER)
      * 010-056 ESPACO VAGO/RESERVADO
      * 057-064 DATA DE ENVIO PARA CONTROLE DE CLASSIFICACAO
      * 065-080 RESERVA PARA USO FUTURO
      *----------------------------------------------------------------*
