server <- function(input, output) {
  
    # Overview section
    output$help <- renderText({
        includeMarkdown("docs/welcome.Rmd")
    })

    output$strata <- renderText({
        includeMarkdown("docs/strata.Rmd")
    })

    output$datasets <- renderText({
        includeMarkdown("docs/datasets.Rmd")
    })

	#output$datasets <- renderDataTable(server=TRUE,{
    #    datatable(datasets, 
    #    caption=paste("INDICATOR DATASETS - We selected a set of biophysical and species indicators for the first phase of the project to demonstrate a sample of what is possible. To be considered, indicators needed to be available as vector or raster datasets covering most or all of boreal region of Canada. Indicators include: biophysical attributes (CMI, GPP, LED, LCC), Songbird species density for 13 species, songbird species core habitat for 13 species, forest songbirds group (density and core habitat), waterfowl species density for 17 species or small species groups, and waterfowl guilds density (cavity nesters, over-water nesters, ground nesters, total waterfowl)."),
    #    rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 20), class="compact")
    #})

    # Leaflet - Ecozone
    output$ecozmap <- renderLeaflet({
        i = ecoz@data[input$indicator]
        if (input$indicator %in% c("CMI","GPP","LED","LCC")) {
            bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
            pal <- colorBin("YlOrRd", domain = i, bins = bins)
        } else {
            bins <- c(0, 0.5, 1, 1.5, 2, 999)
            pal <- colorBin("Spectral", domain = i, bins = bins)
        }
        ecozPopup = paste0("<b>Ecozone</b>: ",ecoz$Ecozone,"<br><b>",input$indicator," KS</b>: ",ecoz[[input$indicator]])
    	p = leaflet(ecoz) %>%
    	  addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
          addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
    	  addPolygons(data=ecoz, fillColor = ~pal(unlist(i)), fill=T, weight=2, color="black", fillOpacity=0.5, group="Ecozone", layerId=~id, popup=ecozPopup)
          #if (input$sfi==TRUE) {
              p = p %>% addRasterImage(sfi, col="blue", opacity=0.5, group="SFI Forests")
          #}
          p = p %>% addLayersControl(position = "topright",
    		baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
    		overlayGroups = c("Ecozone","SFI Forests"),
    		options = layersControlOptions(collapsed = FALSE)) %>%
    	  hideGroup(c("SFI Forests")) %>%
          addLegend(pal = pal, values = ~i, opacity = 0.7, title = input$indicator,
            position = "bottomright")
    })

    # Leaflet - Ecoprovince
    output$ecopmap <- renderLeaflet({
        i = ecop@data[input$indicator]
        if (input$indicator %in% c("CMI","GPP","LED","LCC")) {
            bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
            pal <- colorBin("YlOrRd", domain = i, bins = bins)
        } else {
            bins <- c(0, 0.5, 1, 1.5, 2, 999)
            pal <- colorBin("Spectral", domain = i, bins = bins)
        }
        ecopPopup = paste0("<b>Ecoprovince</b>: ",ecop$Ecoprovince,"<br><b>",input$indicator," KS</b>: ",ecop[[input$indicator]])
    	p = leaflet(ecop) %>%
    	  addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
          addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
    	  addPolygons(fillColor = ~pal(unlist(i)), fill=T, weight=2, color="black", fillOpacity=0.5, group="Ecoprovince", layerId=~id, popup=ecopPopup)
          #if (input$sfi==TRUE) {
              p = p %>% addRasterImage(sfi, col="blue", opacity=0.5, group="SFI Forests")
          #}
          p = p %>% addLayersControl(position = "topright",
    		baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
    		overlayGroups = c("Ecoprovince","SFI Forests"),
    		options = layersControlOptions(collapsed = FALSE)) %>%
    		hideGroup(c("SFI Forests")) %>%
          addLegend(pal = pal, values = ~i, opacity = 0.7, title = input$indicator,
            position = "bottomright")
    })

    # Leaflet - Ecoregion
    output$ecormap <- renderLeaflet({
        i = ecor@data[input$indicator]
        if (input$indicator %in% c("CMI","GPP","LED","LCC")) {
            bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
            pal <- colorBin("YlOrRd", domain = i, bins = bins)
        } else {
            bins <- c(0, 0.5, 1, 1.5, 2, 999)
            pal <- colorBin("Spectral", domain = i, bins = bins)
        }
        ecorPopup = paste0("<b>Ecoregion</b>: ",ecor$Ecoregion,"<br><b>",input$indicator," KS</b>: ",ecor[[input$indicator]])
    	p = leaflet(ecor) %>%
    	  addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
          addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
    	  addPolygons(fillColor = ~pal(unlist(i)), fill=T, weight=2, color="black", fillOpacity=0.5, group="Ecoregion", layerId=~id, popup=ecorPopup)
          #if (input$sfi==TRUE) {
            p = p %>% addRasterImage(sfi, col="blue", opacity=0.5, group="SFI Forests")
          #}
          p = p %>% addLayersControl(position = "topright",
    		baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
    		overlayGroups = c("Ecoregion","SFI Forests"),
    		options = layersControlOptions(collapsed = FALSE)) %>%
    		hideGroup(c("SFI Forests")) %>%
          addLegend(pal = pal, values = ~i, opacity = 0.7, title = input$indicator,
            position = "bottomright")
    })

    # Plot map - Ecozone
    output$ecozplot <- renderPlot({
        r = raster(paste0("../data/raster/", input$indicator, ".tif"))
        v = shapefile('../data/vector/ecoz.shp')
        plot(r, main=paste0("Distribution of ",input$indicator), axes = FALSE, box = TRUE)
        plot(v, add = TRUE)
    })

    # Plot map - Ecoprovince
    output$ecopplot <- renderPlot({
        r = raster(paste0("../data/raster/", input$indicator, ".tif"))
        v = shapefile('../data/vector/ecop.shp')
        plot(r, main=paste0("Distribution of ",input$indicator), axes = FALSE, box = TRUE)
        plot(v, add = TRUE)
    })

    # Plot map - Ecoregion
    output$ecorplot <- renderPlot({
        r = raster(paste0("../data/raster/", input$indicator, ".tif"))
        v = shapefile('../data/vector/ecor.shp')
        plot(r, main=paste0("Distribution of ",input$indicator), axes = FALSE, box = TRUE)
        plot(v, add = TRUE)
    })

    # Table - Ecozone
	output$ecoztable1 <- renderDataTable({
		datatable(x[x$indicator %in% indicators1,1:8], caption = 'Dissimilarity metric for biophysical indicators by ecozone', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
    })
	output$ecoztable2 <- renderDataTable({
		datatable(x[x$indicator %in% indicators2,1:8], caption = 'Representation ratios for songbird species by ecozone', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})
	output$ecoztable4 <- renderDataTable({
		datatable(x[x$indicator %in% indicators4,1:8], caption = 'Representation ratios for songbird core habitat by ecozone', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})
	output$ecoztable5 <- renderDataTable({
		datatable(x[x$indicator %in% indicators5,1:8], caption = 'Representation ratios for waterfowl species by ecozone', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})
	output$ecoztable3 <- renderDataTable({
		datatable(x[x$indicator %in% indicators3,1:8], caption = 'Representation ratios for waterfowl guilds by ecozone', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})

    # Table - Ecoprovince
	output$ecoptable1 <- renderDataTable({
		datatable(x[x$indicator %in% indicators1,c(1,9:20)], caption = 'Dissimilarity metric for biophysical indicators by ecoprovince', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})
	output$ecoptable2 <- renderDataTable({
		datatable(x[x$indicator %in% indicators2,c(1,9:20)], caption = 'Representation ratios for songbird species by ecoprovince', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})
	output$ecoptable4 <- renderDataTable({
		datatable(x[x$indicator %in% indicators4,c(1,9:20)], caption = 'Representation ratios for songbird core habitat by ecoprovince', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})
	output$ecoptable5 <- renderDataTable({
		datatable(x[x$indicator %in% indicators5,c(1,9:20)], caption = 'Representation ratios for waterfowl species by ecoprovince', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})
	output$ecoptable3 <- renderDataTable({
		datatable(x[x$indicator %in% indicators3,c(1,9:20)], caption = 'Representation ratios for waterfowl guilds by ecoprovince', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})

    # Table - Ecoregion
	output$ecortable1 <- renderDataTable({
		datatable(x[x$indicator %in% indicators1,c(1,21:dim(x)[2])], caption = 'Dissimilarity metric for biophysical indicators by ecoregion', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})
	output$ecortable2 <- renderDataTable({
		datatable(x[x$indicator %in% indicators2,c(1,21:dim(x)[2])], caption = 'Representation ratios for songbird species by ecoregion', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})
	output$ecortable4 <- renderDataTable({
		datatable(x[x$indicator %in% indicators4,c(1,21:dim(x)[2])], caption = 'Representation ratios for songbird core habitat by ecoregion', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})
	output$ecortable5 <- renderDataTable({
		datatable(x[x$indicator %in% indicators5,c(1,21:dim(x)[2])], caption = 'Representation ratios for waterfowl species by ecoregion', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})
	output$ecortable3 <- renderDataTable({
		datatable(x[x$indicator %in% indicators3,c(1,21:dim(x)[2])], caption = 'Representation ratios for waterfowl guilds by ecoregion', rownames=F, options=list(dom = 'tip', scrollX = TRUE, pageLength = 12), class="compact") %>%
		formatStyle('indicator', target='row', backgroundColor = styleEqual(input$indicator, 'yellow'))
	})

    # Ecozone map click
    ecoz_click = reactiveValues(clickedMarker=NULL)
    observeEvent(input$ecozmap_shape_click, {
        ecoz_click$clickedShape = input$ecozmap_shape_click
    })
    output$ecoz_barplot = renderPlot({
        ecozone = ecoz_click$clickedShape$id
        x = filter(x, indicator %in% indicators1)
        if (is.null(ecozone)) {ecozone="4"}
        if (ecozone %in% ecoz_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecoz",ecozone)]] <= input$threshold, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecoz",ecozone)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold)
            p + coord_flip() + labs(title=paste("Ecozone",ecozone), x="", y="Dissimilarity metric") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecoz_barplot2 = renderPlot({
        ecozone = ecoz_click$clickedShape$id
        x = filter(x, indicator %in% indicators2)
        if (is.null(ecozone)) {ecozone="4"}
        if (ecozone %in% ecoz_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecoz",ecozone)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecoz",ecozone)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecozone",ecozone), x="", y="Representation ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecoz_barplot3 = renderPlot({
        ecozone = ecoz_click$clickedShape$id
        x = filter(x, indicator %in% indicators3)
        if (is.null(ecozone)) {ecozone="4"}
        if (ecozone %in% ecoz_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecoz",ecozone)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecoz",ecozone)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecozone",ecozone), x="", y="Representation ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecoz_barplot4 = renderPlot({
        ecozone = ecoz_click$clickedShape$id
        x = filter(x, indicator %in% indicators4)
        if (is.null(ecozone)) {ecozone="4"}
        if (ecozone %in% ecoz_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecoz",ecozone)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecoz",ecozone)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecozone",ecozone), x="", y="Representation ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecoz_barplot5 = renderPlot({
        ecozone = ecoz_click$clickedShape$id
        x = filter(x, indicator %in% indicators5)
        if (is.null(ecozone)) {ecozone="4"}
        if (ecozone %in% ecoz_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecoz",ecozone)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecoz",ecozone)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecozone",ecozone), x="", y="Representation ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })

    # Ecoprovince map click
    ecop_click = reactiveValues(clickedMarker=NULL)
    observeEvent(input$ecopmap_shape_click, {
        ecop_click$clickedShape = input$ecopmap_shape_click
    })
    output$ecop_barplot = renderPlot({
        ecoprovince = ecop_click$clickedShape$id
        x = filter(x, indicator %in% indicators1)
        if (is.null(ecoprovince)) {ecoprovince="4.3"}
        if (ecoprovince %in% ecop_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecop",ecoprovince)]] <= input$threshold, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecop",ecoprovince)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold)
            p + coord_flip() + labs(title=paste("Ecoprovince",ecoprovince), x="", y="Dissimilarity metric") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecop_barplot2 = renderPlot({
        ecoprovince = ecop_click$clickedShape$id
        x = filter(x, indicator %in% indicators2)
        if (is.null(ecoprovince)) {ecoprovince="4.3"}
        if (ecoprovince %in% ecop_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecop",ecoprovince)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecop",ecoprovince)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecoprovince",ecoprovince), x="", y="Representation ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecop_barplot3 = renderPlot({
        ecoprovince = ecop_click$clickedShape$id
        x = filter(x, indicator %in% indicators3)
        if (is.null(ecoprovince)) {ecoprovince="4.3"}
        if (ecoprovince %in% ecop_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecop",ecoprovince)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecop",ecoprovince)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecoprovince",ecoprovince), x="", y="Representation ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecop_barplot4 = renderPlot({
        ecoprovince = ecop_click$clickedShape$id
        x = filter(x, indicator %in% indicators4)
        if (is.null(ecoprovince)) {ecoprovince="4.3"}
        if (ecoprovince %in% ecop_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecop",ecoprovince)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecop",ecoprovince)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecoprovince",ecoprovince), x="", y="Representation ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecop_barplot5 = renderPlot({
        ecoprovince = ecop_click$clickedShape$id
        x = filter(x, indicator %in% indicators5)
        if (is.null(ecoprovince)) {ecoprovince="4.3"}
        if (ecoprovince %in% ecop_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecop",ecoprovince)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecop",ecoprovince)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecoprovince",ecoprovince), x="", y="Representation ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })

    # Ecoregion map click
    ecor_click = reactiveValues(clickedMarker=NULL)
    observeEvent(input$ecormap_shape_click, {
        ecor_click$clickedShape = input$ecormap_shape_click
    })
    output$ecor_barplot = renderPlot({
        ecoregion = ecor_click$clickedShape$id
        x = filter(x, indicator %in% indicators1)
        if (is.null(ecoregion)) {ecoregion="64"}
        if (ecoregion %in% ecor_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecor",ecoregion)]] <= input$threshold, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecor",ecoregion)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold)
            p + coord_flip() + labs(title=paste("Ecoregion",ecoregion), x="", y="Dissimilarity metric") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecor_barplot2 = renderPlot({
        ecoregion = ecor_click$clickedShape$id
        x = filter(x, indicator %in% indicators2)
        if (is.null(ecoregion)) {ecoregion="64"}
        if (ecoregion %in% ecor_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecor",ecoregion)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecor",ecoregion)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecoregion",ecoregion), x="", y="Represention ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecor_barplot3 = renderPlot({
        ecoregion = ecor_click$clickedShape$id
        x = filter(x, indicator %in% indicators3)
        if (is.null(ecoregion)) {ecoregion="64"}
        if (ecoregion %in% ecor_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecor",ecoregion)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecor",ecoregion)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecoregion",ecoregion), x="", y="Represention ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecor_barplot4 = renderPlot({
        ecoregion = ecor_click$clickedShape$id
        x = filter(x, indicator %in% indicators4)
        if (is.null(ecoregion)) {ecoregion="64"}
        if (ecoregion %in% ecor_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecor",ecoregion)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecor",ecoregion)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecoregion",ecoregion), x="", y="Represention ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })
    output$ecor_barplot5 = renderPlot({
        ecoregion = ecor_click$clickedShape$id
        x = filter(x, indicator %in% indicators5)
        if (is.null(ecoregion)) {ecoregion="64"}
        if (ecoregion %in% ecor_na) {
            print("Do nothing")
        } else {
            quality = ifelse(x[[paste0("ecor",ecoregion)]] >= input$threshold2, "higher", "lower")
            x = cbind(x, quality)
            p = ggplot(data=x, aes(x=indicator,y=x[[paste0("ecor",ecoregion)]], fill=quality)) + geom_bar(stat="identity") + geom_hline(yintercept=input$threshold2)
            p + coord_flip() + labs(title=paste("Ecoregion",ecoregion), x="", y="Represention ratio") + scale_fill_manual("Representation", values=c("higher"="#636363","lower"="#bdbdbd"))
        }
    })

    # Markdown - Ecozone
	output$ecozdata <- renderText({
		#includeMarkdown(paste0("docs/",input$indicator,".md"))
        includeHTML(paste0("docs/ecozone.html"))
    })

    # Markdown - Ecoprovince
	output$ecopdata <- renderText({
		includeHTML(paste0("docs/ecoprovince.html"))
    })

    # Markdown - Ecoregion
	output$ecordata <- renderText({
		includeHTML(paste0("docs/ecoregion.html"))
    })

    # Methods section
	output$methods <- renderText({
		includeMarkdown("docs/methods.Rmd")
    })
}
