
      *-----------------------------------------------------------------
      *  I#BVVEB9 - MENSAGEM DE ERRO RECEBIDAS DA CIP - ASLC
      *----------------------------------------------------------------

       01  BVVEB9-MENSAGENS.
           03 FILLER                   PIC X(008)  VALUE
             'ESLC0006'.
           03 FILLER                   PIC X(100)  VALUE
             'DATA INV·LIDA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0007'.
           03 FILLER                   PIC X(100)  VALUE
             'CPF OU CNPJ INV·LIDO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0029'.
           03 FILLER                   PIC X(100)  VALUE
             'REQUISIÁ„O ENVIADA FORA DA GRADE HOR·RIA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0041'.
           03 FILLER                   PIC X(100)  VALUE
             'GRADE HOR·RIA N„O CADASTRADA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0042'.
           03 FILLER                   PIC X(100)  VALUE
             'N„O EXISTE ADES„O PARA ESSA FUNCIONALIDADE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0088'.
           03 FILLER                   PIC X(100)  VALUE
             'TIPO DE PESSOA CENTRALIZADORA FORA DO DOMÌNIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0089'.
           03 FILLER                   PIC X(100)  VALUE
             'CÛDIGO DA MOEDA FORA DO DOMÌNIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0090'.
           03 FILLER                   PIC X(100)  VALUE
             'TIPO DE PESSOA PONTO DE VENDA FORA DO DOMÌNIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0091'.
           03 FILLER                   PIC X(100)  VALUE
             'CÛDIGO DO INSTITUIDOR DO ARRANJO DE PAGAMENTO FORA DO DOMÌ
      -      'NIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0092'.
           03 FILLER                   PIC X(100)  VALUE
             'CÛDIGO DA OCORRÍNCIA FORA DO DOMÌNIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0097'.
           03 FILLER                   PIC X(100)  VALUE
             'NU LIQUIDAÁ„O N„O CADASTRADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0102'.
           03 FILLER                   PIC X(100)  VALUE
             'CNPJ BASE CREDENCIADOR DIFERENTE DO CADASTRADO NO SLC.'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0103'.
           03 FILLER                   PIC X(100)  VALUE
             'CNPJ CREDENCIADOR DIFERENTE DO CADASTRADO NO SLC'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0104'.
           03 FILLER                   PIC X(100)  VALUE
             'N˙MERO CONTROLE CREDENCIADOR CENTRALIZADORA N„O PODE SER R
      -      'EPETIDO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0105'.
           03 FILLER                   PIC X(100)  VALUE
             'N˙MERO CONTROLE CREDENCIADOR PONTO VENDA N„O PODE SER REPE
      -      'TIDO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0106'.
           03 FILLER                   PIC X(100)  VALUE
             'IF CREDORA SEM ADES„O A FUNCIONALIDADE: LIQUIDAÁıES DAS TR
      -      'ANSAÁıES DE CARTıES DE CRÈDITO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0107'.
           03 FILLER                   PIC X(100)  VALUE
             'TIPO DE PRODUTO LIQUIDAÁ„O DE CRÈDITO FORA DO DOMÌNIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0108'.
           03 FILLER                   PIC X(100)  VALUE
             'INDICADOR DA FORMA DE TRANSFERÍNCIA FORA DO DOMÌNIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0109'.
           03 FILLER                   PIC X(100)  VALUE
             'N˙MERO CONTROLE IF N„O PODE SER REPETIDO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0110'.
           03 FILLER                   PIC X(100)  VALUE
             'IF CREDORA SEM ADES„O A FUNCIONALIDADE: LIQUIDAÁıES DAS TR
      -      'ANSAÁıES DE CARTıES DE DÈBITO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0111'.
           03 FILLER                   PIC X(100)  VALUE
             'TIPO DE PRODUTO LIQUIDAÁ„O DE DÈBITO FORA DO DOMÌNIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0112'.
           03 FILLER                   PIC X(100)  VALUE
             'DATA DE PAGAMENTO INFORMADA N„O PERMITIDA PARA O TIPO DE P
      -      'RODUTO CART„O DE CRÈDITO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0113'.
           03 FILLER                   PIC X(100)  VALUE
             'DATA DE PAGAMENTO INFORMADA N„O PERMITIDA PARA O TIPO DE P
      -      'RODUTO IGUAL A AJUSTES DE CRÈDITO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0114'.
           03 FILLER                   PIC X(100)  VALUE
             'DATA DE PAGAMENTO INFORMADA N„O PERMITIDA PARA O TIPO DE P
      -      'RODUTO LIQUIDAÁ„O DE DÈBITO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0115'.
           03 FILLER                   PIC X(100)  VALUE
             'ERRO NO SEGMENTO ESTARQ, CAMPO N„O PREENCHIDO.'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0116'.
           03 FILLER                   PIC X(100)  VALUE
             'ERRO NO SEGMENTO ESTARQ, FORMATO DO DADO INV·LIDO.'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0117'.
           03 FILLER                   PIC X(100)  VALUE
             'VALOR DO PAGAMENTO DEVE SER MAIOR QUE ZERO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0118'.
           03 FILLER                   PIC X(100)  VALUE
             'PARTICIPANTE N„O CADASTRADO NO SLC'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0119'.
           03 FILLER                   PIC X(100)  VALUE
             'PARTICIPANTE ADMINISTRADO N„O È ADMINISTRADO PELO PARTICIP
      -      'ANTE PRINCIPAL'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0120'.
           03 FILLER                   PIC X(100)  VALUE
             'INFORMAR OS CAMPOS AGÍNCIA E CONTA CENTRALIZADORA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0121'.
           03 FILLER                   PIC X(100)  VALUE
             'TAG TIPO CONTA N„O INFORMADA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0122'.
           03 FILLER                   PIC X(100)  VALUE
             'ISPB INFORMADO N„O POSSUI RELACIONAMENTO COM O CREDENCIADO
      -      'R'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0123'.
           03 FILLER                   PIC X(100)  VALUE
             'PARTICIPANTE N„O ADERIU A FUNCIONALIDADE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0125'.
           03 FILLER                   PIC X(100)  VALUE
             'TIPO DE CONTA INFORMADA FORA DO DOMÌNIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0126'.
           03 FILLER                   PIC X(100)  VALUE
             'CAMPO OBRIGATÛRIO N„O PREENCHIDO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0127'.
           03 FILLER                   PIC X(100)  VALUE
             'DADOS ENVIADOS N„O CORRESPONDEM AO TIPO DE CONTA INFORMADO
      -      ' '.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0128'.
           03 FILLER                   PIC X(100)  VALUE
             'DATA DE INFORMADA N„O PERMITIDA PARA O TIPO DE PRODUTO LIQ
      -      'UIDAÁ„O DE CARTıES'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0129'.
           03 FILLER                   PIC X(100)  VALUE
             'NU LIQUIDAÁ„O EM PROCESSO DE LIQUIDAÁ„O'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0130'.
           03 FILLER                   PIC X(100)  VALUE
             'PRAZO PARA RECEBIMENTO DO N˙MERO DE LIQUIDAÁ„O VENCIDO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0131'.
           03 FILLER                   PIC X(100)  VALUE
             'ISPB DO EMISSOR N„O PODE SER DIFERENTE DO IDENTIFICADOR DO
      -      ' PARTICIPANTE PRINCIPAL'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0132'.
           03 FILLER                   PIC X(100)  VALUE
             'N„O EXISTE ADES„O CADASTRADA PARA O ARRANJO INFORMADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0133'.
           03 FILLER                   PIC X(100)  VALUE
             'ISPB DEVEDORA INFORMADA N„O EST· VIGENTE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0134'.
           03 FILLER                   PIC X(100)  VALUE
             'CÛDIGO DE OCORRÍNCIA INFORMADO N„O PERMITIDO PARA O PRIMEI
      -      'RO RETORNO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0135'.
           03 FILLER                   PIC X(100)  VALUE
             'SITUAÁ„O DO LANÁAMENTO N„O PERMITE ATUALIZAÁ„O'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0136'.
           03 FILLER                   PIC X(100)  VALUE
             'CÛDIGO DE OCORRÍNCIA N„O PERMITIDO PARA O TIPO DE LIQUIDAÁ
      -      '„O DO LANÁAMENTO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0137'.
           03 FILLER                   PIC X(100)  VALUE
             'CÛDIGO DE OCORRÍNCIA N„O PERMITIDO PARA LANÁAMENTO N„O LIQ
      -      'UIDADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0138'.
           03 FILLER                   PIC X(100)  VALUE
             'INDICADOR DE FORMA DE TRANSFERÍNCIA N„O PERMITIDO PARA O A
      -      'RRANJO DE PAGAMENTO INFORMADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0139'.
           03 FILLER                   PIC X(100)  VALUE
             'N„O EXISTE RELACIONAMENTO CADASTRADO PARA O PARTICIPANTE X
      -      ' ARRANJO DE PAGAMENTO INFORMADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0140'.
           03 FILLER                   PIC X(100)  VALUE
             'CÛDIGO INSTITUIDOR DE ARRANJO DE PAGAMENTO INFORMADO N„O P
      -      'ERMITIDO PARA O TIPO DE ARQUIVO ENVIADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0141'.
           03 FILLER                   PIC X(100)  VALUE
             'CNPJ BASE DO CREDENCIADOR INFORMADO DIVERGENTE DO INFORMAD
      -      'O NA TAG PARTICIPANTE ADMINISTRADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0142'.
           03 FILLER                   PIC X(100)  VALUE
             'QUANTIDADE M·XIMA DE ARQUIVOS EXCEDIDA PARA A DATA DE PROC
      -      'ESSAMENTO '.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0143'.
           03 FILLER                   PIC X(100)  VALUE
             'QUANTIDADE M·XIMA DE REGISTROS EXCEDIDA PARA O MEIO DE TRA
      -      'NSMISS„O'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0144'.
           03 FILLER                   PIC X(100)  VALUE
             'A DATA INFORMADA È DIFERENTE DA DATA DE REFERÍNCIA DO SIST
      -      'EMA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0145'.
           03 FILLER                   PIC X(100)  VALUE
             'NOME DO ARQUIVO J· REGISTRADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0146'.
           03 FILLER                   PIC X(100)  VALUE
             'CÛDIGO DE OCORRÍNCIA DE SUCESSO E ERRO N„O PERMITIDO NO ME
      -      'SMO RETORNO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0147'.
           03 FILLER                   PIC X(100)  VALUE
             'ISPB LIQUIDAÁ„O PONTO DE VENDA N„O PODE SER DIFERENTE DO I
      -      'SPB IF CREDORA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0148'.
           03 FILLER                   PIC X(100)  VALUE
             'ISPB IF CREDORA N„O POSSUI LIQUIDANTE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0149'.
           03 FILLER                   PIC X(100)  VALUE
             'VALOR DO PAGAMENTO EXCEDIDO O M·XIMO PERMITIDO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

           03 FILLER                   PIC X(008)  VALUE
             'ESLC0999'.
           03 FILLER                   PIC X(100)  VALUE
             'ERRO INESPERADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.

       01  BVVEB9-MENSAGENS-R          REDEFINES
           BVVEB9-MENSAGENS.
           03 FILLER                   OCCURS 99.
               05 BVVEB9-COD-MSG       PIC X(008).
               05 BVVEB9-TXT-MSG       PIC X(100).
               05 BVVEB9-ACU-MSG       PIC 9(009) COMP-3.

