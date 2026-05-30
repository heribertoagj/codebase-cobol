      ******************************************************************
      * NOME BOOK : PSDCWAXL - INTERFACE DADOS DE ENTRADA              *
      * DESCRICAO :                                                    *
      * DATA      : 21/02/2011                                         *
      * AUTOR     : ADEMIR BARBOSA JUNIOR                              *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : CPM BRAXIS                                         *
      * COMPONENTE:                                                    *
      * TAMANHO   : 13208 BYTES                                        *
      *********************** DADOS DE ENTRADA *************************
      *                                                                *
      * CEMPR.............: CODIGO DA EMPRESA                          *
      * CCLUB.............: CODIGO DO CLUB                             *
      * CPFCNPJ...........: NUMERO DO CPF / CNPJ                       *
      * CFLIAL-CPFCNPJ....: CODIGO DA FILIAL DO CPF / CNPJ             *
      * CTRL-CPFCNPJ......: CODIGO DE CONTROLE DO CPF / CNPJ           *
      * TPO-PSSOA.........: TIPO DE PESSOA                             *
      *                       PESSOA FISICA   'F'                      *
      *                       PESSOA JURIDICA 'J'                      *
      * CCLASS-CAD-PSSOA  : CODIGO DA CLASSE CADASTRAL DA PESSOA       *
      * NOME-RSOCIAL      : NOME DA PESSOA OU RAZAO SOCIAL             *
      * DNASC-FUNDC       : DATA DE NASCIMENTO OU FUNDAÇÃO             *
      * CTPO-TTLAR-CPF    : TITULAR DO CPF                             *
      * DULT-ATULZ-CAD    : INDICADOR DE LIMITE DE CREDITO             *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 PSDCWAXL-HEADER.
             10 PSDCWAXL-COD-LAYOUT    PIC X(08)   VALUE 'PSDCWAXL'.
             10 PSDCWAXL-TAM-LAYOUT    PIC 9(05)   VALUE  13208.
           05 PSDCWAXL-REGISTRO.
             10 PSDCWAXL-BLOCO-RETORNO.
               15 PSDCWAXL-COD-RETORNO             PIC 9(02).
               15 PSDCWAXL-COD-ERRO                PIC X(04).
               15 PSDCWAXL-COD-MENSAGEM            PIC X(08).
               15 PSDCWAXL-DESC-MENSAGEM           PIC X(50).
             10 PSDCWAXL-BLOCO-ENTRADA.
               15 PSDCWAXL-E-CEMPR                 PIC 9(10).
               15 PSDCWAXL-E-CPFCNPJ               PIC 9(09).
               15 PSDCWAXL-E-CFLIAL-CPFCNPJ        PIC 9(04).
               15 PSDCWAXL-E-CTRL-CPFCNPJ          PIC 9(02).
               15 PSDCWAXL-E-TPO-PSSOA             PIC X(01).
             10 PSDCWAXL-BLOCO-SAIDA.
               15 PSDCWAXL-NUM-CONSULTAS           PIC 9(05).
               15 PSDCWAXL-S-REG-SAIDA  OCCURS 0 TO 100 TIMES
                         DEPENDING ON PSDCWAXL-NUM-CONSULTAS.
                 20 PSDCWAXL-S-CEMPR               PIC 9(10).
                 20 PSDCWAXL-S-CCLUB               PIC 9(10).
                 20 PSDCWAXL-S-CPFCNPJ             PIC 9(09).
                 20 PSDCWAXL-S-CFLIAL-CPFCNPJ      PIC 9(04).
                 20 PSDCWAXL-S-CTRL-CPFCNPJ        PIC 9(02).
                 20 PSDCWAXL-S-TPO-PSSOA           PIC X(01).
                 20 PSDCWAXL-S-CCLASS-CAD-PSSOA    PIC 9(03).
                 20 PSDCWAXL-S-NOME-RSOCIAL        PIC X(70).
                 20 PSDCWAXL-S-DNASC-FUNDC         PIC X(10).
                 20 PSDCWAXL-S-CTPO-TTLAR-CPF      PIC 9(02).
                 20 PSDCWAXL-S-DULT-ATULZ-CAD      PIC X(10).
