      ******************************************************************
      *    I#BVVECD - TABELA DE DESCRICOES DOS ERROS ENCONTRADOS NA    *
      *               CONSISTENCIA DO ARQUIVO DE CONFIRMACOES DE       *
      *               TITULARIDADE ENVIADO PELA REDECARD               *
      *    LRECL: 2040                                                 *
      ******************************************************************
       01  BVVECD-TB-ERROS-CONSISTENCIA.
           05  FILLER                   PIC X(85) VALUE
              'TIPO DE REGISTRO INVALIDO                         '.
           05  FILLER                   PIC X(85) VALUE
              'BANCO DE ENVIO DIFERENTE DE 237                   '.
           05  FILLER                   PIC X(85) VALUE
              'IDENTIFICACAO DA PROPOSTA DE AFILIACAO INVALIDO   '.
           05  FILLER                   PIC X(85) VALUE
              'NUMERO SEQUENCIAL NAO NUMERICO OU ZERADO          '.
           05  FILLER                   PIC X(85) VALUE
              'DATA DO PROCESSAMENTO INVALIDA                    '.
           05  FILLER                   PIC X(85) VALUE
              'HORA DO PROCESSAMENTO INVALIDA                    '.
           05  FILLER                   PIC X(85) VALUE
              'TIPO DE PESSOA INVALIDO                           '.
           05  FILLER                   PIC X(85) VALUE
              'CNPJ/CPF DO ESTAB. NAO NUMERICO OU ZERADO         '.
           05  FILLER                   PIC X(85) VALUE
              'STATUS CNPJ/CPF DIFERENTE DE ZEROS                '.
           05  FILLER                   PIC X(85) VALUE
              'AGENCIA DA CONTA CORRENTE NAO NUMERICA/ZERADA     '.
           05  FILLER                   PIC X(85) VALUE
              'DIGITO DA AGENCIA INVALIDO                        '.
           05  FILLER                   PIC X(85) VALUE
              'STATUS DA AGENCIA INVALIDO                        '.
           05  FILLER                   PIC X(85) VALUE
              'NUMERO DA CONTA CORRENTE NAO NUMERICO/ZERADO      '.
           05  FILLER                   PIC X(85) VALUE
              'DIGITO DA CONTA CORRENTE INVALIDO                 '.
           05  FILLER                   PIC X(85) VALUE
              'STATUS DA CONTA CORRENTE DIFERENTE DE ZEROS       '.
           05  FILLER                   PIC X(85) VALUE
              'ESTAGIO DA VALIDACAO INVALIDO                     '.
           05  FILLER                   PIC X(85) VALUE
              'TIPO DE CONTA INVALIDO                            '.
           05  FILLER                   PIC X(85) VALUE
              'RAZAO SOCIAL INVALIDA                             '.
           05  FILLER                   PIC X(85) VALUE
              'CEP INVALIDO                                      '.
           05  FILLER                   PIC X(85) VALUE
              'TIPO DE DOMICILIO INVALIDO                        '.
           05  FILLER                   PIC X(85) VALUE
              'DATA DE ENVIO INVALIDA                            '.
           05  FILLER                   PIC X(85) VALUE
              'QUANTIDADE DE REGISTROS INFORMADA NO TRAILLER DIFERENTE D
      -       'A QUANTIDADE DE DETALHES'.
           05  FILLER                   PIC X(85) VALUE
              '*** HEADER INEXISTENTE NO ARQUIVO CIELO ***       '.
           05  FILLER                   PIC X(85) VALUE
              '*** TRAILLER INEXISTENTE NO ARQUIVO CIELO ***     '.

       01  FILLER REDEFINES BVVECD-TB-ERROS-CONSISTENCIA.
           05  BVVECD-TB-ERRODESCR      PIC X(85) OCCURS 24.
