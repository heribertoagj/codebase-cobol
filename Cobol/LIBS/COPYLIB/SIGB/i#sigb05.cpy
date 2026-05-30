***************************** Top of Data ******************************
      ****************************************************************
      *                                                              *
      *          ARQUIVO RECEPCAO INTERFACES DO SISTEMA SIGB         *
      *                                                              *
      *              - INC   I#SIGB05             LRECL=50           *
      ****************************************************************

       01  SIGB-ARQ-ALIMENTADOR.
           02  SIGB-AGENCIA              PIC S9(005) COMP-3.
           02  SIGB-CONTA                PIC S9(007) COMP-3.
           02  SIGB-CGCCPF-PRINC         PIC S9(009) COMP-3.
           02  SIGB-CGCCPF-FILIAL        PIC S9(005) COMP-3.
           02  SIGB-CGCCPF-DAC           PIC S9(003) COMP-3.
           02  SIGB-CCUSTO               PIC  X(004).
           02  SIGB-COD-CART             PIC S9(005) COMP-3.
           02  SIGB-DATAREF-CART         PIC S9(009) COMP-3.
           02  SIGB-DADO1.
               03  SIGB-DADO-CONS1       PIC S9(015) COMP-3.
               03  SIGB-FILLER1          PIC  X(013).
           02  SIGB-DADO2  REDEFINES     SIGB-DADO1.
               03  SIGB-DADO-CONS2       PIC S9(001) COMP-3.
               03  SIGB-FILLER2          PIC  X(020).

*******OBS. 1 - CLASSIFICACAO: AGENCIA, CONTA CORRENTE, CGC/CPF.
*******
*******OBS. 2 - O CPF/CNPJ CONSTANTE DO ARQUIVO DEVERA SER SEMPRE
*******         O DO PRIMEIRO TITULAR DA CONTA.
*******
*******OBS. 3 - PARA A RECEPCAO DESTE ARQUIVO, O RESPECTIVO
*******         CODIGO DE CARTEIRA E CENTRO DE CUSTO DEVEM ESTAR
*******         PREVIAMENTE CADASTRADOS NO SISTEMA SIGB.
*******
*******OBS. 4 - FORMATO CAMPO DATAREF : AAAAMMDD.
*******         A DATA DEVE SER A MESMA PARA TODOS OS REGISTROS.
*******
*******OBS. 5 - PARA INFORMACOES DO TIPO VALOR/QUANTIDADE,
*******         UTILIZAR DADO1.
*******         NO CASO DE VALOR NAO INFORMAR CASAS DECIMAIS.
*******
*******OBS. 6 - PARA INFORMACOES DO TIPO SINALIZADOR,
*******         UTILIZAR DADO2, CUJO CONTEUDO SERA FIXO 1.
*******
*******OBS. 7 - PARA REGISTRO DE TOTAL (TRAILER) PROCEDER:
*******         CAMPOS AGENCIA, CONTA, E  CGC/CPF  CONTEUDO 9'S.
*******         CAMPO   DADO1 - QTDADE DE  REGISTROS DO ARQUIVO,
*******                         (EXCETO O REGISTRO DE TOTAL).
*******
