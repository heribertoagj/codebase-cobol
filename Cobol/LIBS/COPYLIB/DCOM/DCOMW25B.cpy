      ******************************************************************
      * NOME BOOK : DCOMW25B - BOOK DE INTERFACE COM SERVICO DCOM425B  *
      * DESCRICAO : CONSULTA INFORMACOES                               *
      * DATA      : 16/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 00147 BYTES                                        *
      *----------------------------------------------------------------*
      * DCOMW25B-E-TPO-PESQUISA = 1 - PESQUISA ORIGINAL TOTAL          *
      *                           2 - PESQUISA ORIGINAL PARCIAL-01     *
      ******************************************************************
           05 DCOMW25B-HEADER.
             10 DCOMW25B-COD-LAYOUT      PIC X(08) VALUE 'DCOMW25B'.
             10 DCOMW25B-TAM-LAYOUT      PIC 9(05) VALUE 00147.
           05 DCOMW25B-REGISTRO.
             10 DCOMW25B-AREA-ENTRADA.
                15 DCOMW25B-E-TPO-PESQUISA            PIC 9(002).
                15 DCOMW25B-E-DANO-OPER-DESC          PIC 9(004).
                15 DCOMW25B-E-NSEQ-OPER-DESC          PIC 9(009).
                15 DCOMW25B-E-HATULZ                  PIC X(026).
             10 DCOMW25B-AREA-SAIDA.
                15 DCOMW25B-S-DANO-OPER-DESC          PIC 9(004).
                15 DCOMW25B-S-NSEQ-OPER-DESC          PIC 9(009).
                15 DCOMW25B-S-HATULZ                  PIC X(026).
                15 DCOMW25B-S-VCUSTO-DESC-DIA         PIC 9(008)V9(07).
                15 DCOMW25B-S-VCUSTO-DESC-MES         PIC 9(008)V9(07).
                15 DCOMW25B-S-VCUSTO-DESC-ANO         PIC 9(008)V9(07).
                15 DCOMW25B-S-CFUNC-BDSCO             PIC 9(009).
